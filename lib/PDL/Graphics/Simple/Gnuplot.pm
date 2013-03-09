######################################################################
######################################################################
######################################################################
###
###
### Gnuplot backend for PDL::Graphics:Simple
###
### See the PDL::Graphics::Simple docs for details
###
##
#
package PDL::Graphics::Simple::Gnuplot;

use File::Temp qw/tempfile/;
use PDL::Options q/iparse/;

our $mod = {
    shortname => 'gnuplot',
    module=>'PDL::Graphics::Simple::Gnuplot',
    engine => 'PDL::Graphics::Gnuplot',
    synopsis=> 'Gnuplot 2D/3D (versatile; beautiful output)',
    pgs_version=> '0.004'
};
PDL::Graphics::Simple::register( 'PDL::Graphics::Simple::Gnuplot' );


##########
# PDL::Graphics::Simple::Gnuplot::check
# Checker
sub check {
    my $force = shift;
    $force = 0 unless(defined($force));

    return $mod->{ok} unless( $force or !defined($mod->{ok}) );

    eval 'use PDL::Graphics::Gnuplot;';
    if($@) {
	$mod->{ok} = 0;
	$mod->{msg} = $@;
	return 0;
    }
    my $gpw;
    eval '$gpw = gpwin();';
    if($@) {
	$mod->{ok} = 0;
	$mod->{msg} = $@;
	die "PDL::Graphics::Simple: PDL::Graphics::Gnuplot didn't construct properly.\n\t$@";
    }
    $mod->{valid_terms} = $gpw->{valid_terms};
    $mod->{ok} = 1;
    return 1;
}


##########
# PDL::Graphics::Simple::Gnuplot::new
# Constructor
our $new_defaults = {
    size => [6,4.5,'in'],
    type => '',
    output => '',
    multi=>undef
};


sub new {
    my $class = shift;
    my $opt_in = shift;
    $opt_in = {} unless(defined($opt_in));
    my $opt = { iparse( $new_defaults, $opt_in ) };
    my $gpw;

    # Force a recheck on failure, in case the user fixed gnuplot.
    # Also loads PDL::Graphics::Gnuplot.
    unless(check()) {
	die "$mod->{shortname} appears nonfunctional\n" unless(check(1));
    }

    # Generate the @params array to feed to gnuplot
    my @params;
    push( @params, "size" => $opt->{size} );
    
    # tempfile gets set if we need to write to a temporary file for image conversion
    my $conv_tempfile = '';
    
    # Do different things for interactive and file types
    if($opt->{type} =~ m/^i/i) {
	push(@params, "title"=>$opt->{output}) if(defined($opt->{output}));

	# Interactive - try WXT, X11 in that order.
	# (aqua doesn't have a "persist" option we can set to 0)
	if($mod->{itype}) {
	    $gpw = gpwin($mod->{itype}, @params, persist=>0 );
	    print $PDL::Graphics::Gnuplot::last_plotcmd;
	} else {
	    attempt:for my $try( 'wxt', 'x11' ) {
		eval { $gpw = gpwin($try, @params, persist=>0 ); };
		last attempt if($gpw);
	    }
	    die "Couldn't start a gnuplot interactive window" unless($gpw);
	    $mod->{itype} = $gpw->{terminal};
	}
    } else {
	# File output - parse out file type, and then see if we support it.
	# (Maybe the parsing part could be pushed into a utility routine...)

	# Filename extension -- 2-4 characters 
	my $ext;
	if($opt->{output} =~ m/\.(\w{2,4})$/) {
	    $ext = $1;
	} else {
	    $ext = '.png';
	    print STDERR "PDL::Graphics::Simple::Gnuplot:  Warning - defaulting to .png type for file '$opt->{output}'\n";
	}
	$opt->{ext} = $ext;

	# Now $ext has the file type - check if its a supported type.  If not, make a 
	# tempfilename to hold gnuplot's output.
	unless($mod->{valid_terms}->{$ext}) {
	    unless($gpw->{valid_terms}->{'png'}) {
		die "PDL::Graphics::Simple: $ext isn't a valid output file type for your gnuplot,\n\tand it doesn't support .png either.  Sorry, I give up.\n";
	    }

	    # Term is invalid but png is supported - set up a tempfile for conversion.
	    my($fh);
	    ($fh,$conv_tempfile) = tempfile('pgs_gnuplot_XXXX');
	    close $fh;
	    unlink($conv_tempfile); # just to be sure;
	    $conv_tempfile .= ".png";
	}
	push( @params, "output" => ($conv_tempfile || $opt->{output}) );
	$gpw = gpwin( $conv_tempfile ? 'png' : $ext,  @params );
    }


    my $me = { opt => $opt, conv_fn => $conv_tempfile, obj=>$gpw };

    # Deal with multiplot setup...
    if(defined($opt->{multi})) {
	$me->{nplots} = $opt->{multi}->[0] * $opt->{multi}->[1];
	$me->{plot_no} = 0;
    } else {
	$me->{nplots} = 0;
    }

    return bless($me, 'PDL::Graphics::Simple::Gnuplot');
}


##############################
# PDL::Graphics::Simple::Gnuplot::plot 
# plot

our $curve_types = {
    points    => 'points',
    lines     => 'lines',
    bins      => 'histeps',
    errorbars => 'yerrorbars',
    limitbars => 'yerrorbars',
    image     => 'image',
    circles   => sub {
	my($me, $po, $co, @data) = @_;
	my $ang = PDL->xvals(362)*3.14159/180;
	my $c = $ang->cos;
	my $s = $ang->sin;
	$s->slice("361") .= $c->slice("361") .= PDL->pdl(1.1)->acos;  # NaN
	my $dr = $data[2]->flat;
	my $dx = ($data[0]->flat->slice("*1") + $dr->slice("*1") * $c)->flat;
	my $dy = ($data[1]->flat->slice("*1") + $dr->slice("*1") * $s)->flat;
	$co->{with} = "lines";
	return [ $co, $dx, $dy ];
    }

};
    
sub plot {
    my $me = shift;
    my $ipo = shift;

    my $po = {
	title    => $ipo->{title},
	xlab     => $ipo->{xlabel},
	ylab     => $ipo->{ylabel},
	key      => $ipo->{key},
	xrange   => $ipo->{xrange},
	yrange   => $ipo->{yrange},
	cbrange  => $ipo->{crange},
	colorbox => $ipo->{wedge},
	justify  => $ipo->{justify} ? $ipo->{justify} : undef,
	clut   => 'sepia',
    };

    $po->{logscale} = [$ipo->{logaxis}] if($ipo->{logaxis});


    my @arglist = ($po);
    for my $block(@_) {
	my $ct = $curve_types->{  $block->[0]->{with}  };
	unless(defined($ct)) {
	    die "PDL::Graphics::Simple::Gnuplot: undefined curve type $ct";
	}
	if(ref($ct) eq 'CODE') {
	    $block = &$ct($me, $po, @$block);
	} else {
	    $block->[0]->{with} = $ct;
	}
	push(@arglist, (@$block));
    }

    if($me->{nplots}) {
	print "nplots...\n";
	unless($me->{plot_no}) {
	    print "issue multiplot...\n";
	    $me->{obj}->multiplot( layout=>[$me->{opt}->{multi}->[0], $me->{opt}->{multi}->[1]] );
	}
    }

    if($ipo->{oplot}) {
	print "replotting\n";
	delete $po->{logaxis};
	delete $po->{xrange};
	delete $po->{yrange};
	delete $po->{cbrange};
	delete $po->{justify};
	$me->{obj}->replot(@arglist);
    } else {
	print "plotting\n";
	$me->{obj}->plot(@arglist);
    }

    if($me->{conv_fn}) {
	print "converting $me->{conv_fn} to $me->{opt}->{output}...";
	$a = rim($me->{conv_fn});
	wim($a, $me->{opt}->{output});
	unlink($me->{conv_fn});
    }

    if($me->{nplots}) {
	$me->{plot_no}++;
	if($me->{plot_no} >= $me->{nplots}) {
	    $me->{obj}->end_multi();
	    $me->{plot_no} = 0;
	}
    }
}
