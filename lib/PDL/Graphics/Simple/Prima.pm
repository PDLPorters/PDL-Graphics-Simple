######################################################################
######################################################################
######################################################################
###
###
### Prima backend for PDL::Graphics:Simple
###
### See the PDL::Graphics::Simple docs for details
###
### Prima setup is borrowed from D. Mertens' PDL::Graphics::Prima::Simple 
###  
##
#

# Still to do:
#       - plot justification
#	- label style plots
#	- line dashes (if possible)
#	- multiplots
#	- file output


package PDL::Graphics::Simple::Prima;
use strict;

use PDL;
use PDL::Options q/iparse/;
our $mod = {
    shortname => 'prima',
    module => 'PDL::Graphics::Simple::Prima',
    engine => 'PDL::Graphics::Prima',
    synopsis => 'Prima (interactive, fast, PDL-specific)',
    pgs_version => '1.000'
};
PDL::Graphics::Simple::register('PDL::Graphics::Simple::Prima');


##########
# PDL::Graphics::Simple::Prima::check
# Checker
sub check {
    my $force = shift;
    $force = 0 unless(defined($force));
    
    return $mod->{ok} unless( $force or !defined($mod->{ok}));
    $mod->{ok} = 0; # makes default case simpler

    # Check Prima availability
    my $min_version = 0.13;
    eval { require PDL::Graphics::Prima; };
    if($@) {
	$mod->{msg} = "Couldn't load PDL::Graphics::Prima: ".$@;
	undef $@;
	return 0;
    }
    if ($PDL::Graphics::Prima::VERSION < $min_version) {
	$mod->{msg} = "Prima version $PDL::Graphics::Prima::VERSION is too low ($min_version required)";
	return 0;
    }

    eval { require PDL::Graphics::Prima::Simple; };
    if($@) {
	$mod->{msg} = "Couldn't load PDL::Graphics::Prima::Simple: ".$@;
	undef $@;
	return 0;
    }
    
    eval {
	require Prima::Application;
	Prima::Application->import();
    };
    if($@) {
	$mod->{msg} = "Couldn't load Prima application: ".$@;
	undef $@;
	return 0;
    }

    # Don't know if all these are actually needed; I'm stealing from the demo.
    # --CED
    eval {
	require Prima::Label;
	require Prima::PodView;
	require Prima::Buttons;
	require Prima::Utils;
	require Prima::Edit;
    };
    if($@){ 
	$mod->{msg} = "Couldn't load auxiliary Prima modules: ".$@;
	undef $@;
	return 0;
    }
 
    $mod->{ok} =1;
    return 1;
}


##############################
# New - constructor
our $new_defaults = {
    size => [6,4.5,'in'],
    type=>'i',
    output=>'',
    multi=>undef
};

## Much of this boilerplate is stolen from PDL::Graphics::Prima::Simple...
our $N_windows = 0;

sub new {
    my $class = shift;
    my $opt_in = shift;
    $opt_in = {} unless(defined($opt_in));
    my $opt = { iparse($new_defaults, $opt_in) };

    if($opt->{type} =~ m/^f/) {
	die "PDL::Graphics::Simple doesn't support Prima file output (yet) -- coming soon!\n";
    }

    if(defined($opt->{multi})) {
	die "PDL::Graphics::Simple doesn't support multiplots on Prima yet -- coming soon!\n";
    }
    
    unless( check() ) {
	die "$mod->{shortname} appears nonfunctional\n" unless(check(1));
    }

    my $size = PDL::Graphics::Simple::_regularize_size($opt->{size},'px');

    my $pw = Prima::Window->create( text => $opt->{output} || "PDL/Prima Plot",
				    size => [$size->[0], $size->[1]],
				    onCreate => sub { $N_windows++; },  
				    onDestroy => sub { $N_windows--;}   # Should maybe do something smarter here --like 
	);                                                              # auto-deallocate from the widgets list...

    my $me = { obj => $pw, widgets => [] };
    return bless($me, "PDL::Graphics::Simple::Prima");
}

sub DESTROY {
    my $me = shift;
    $me->{obj}->hide;
    $me->{obj}->destroy;
}


our @colors =qw/
    cl::Black cl::Red cl::Green cl::Blue cl::Cyan cl::Magenta cl::Yellow cl::Brown cl::LighttRed cl::LightGreen cl::LightBlue cl::Gray/;

##############################
# Fake-o apply method makes sepiatone values for input data.
# We have to mock up an object method to match the style of PDL::Graphics::Prima::Palette,
# in order to make the Matrix plot type happy (for 'with=>image').
@PDL::Graphics::Simple::Prima::Bogus_palette::ISA = 'PDL::Graphics::Prima::Palette';
sub PDL::Graphics::Simple::Prima::Bogus_palette::apply {
    my $h = shift;
    my $data = shift;

    my $me = $h->{me};
    my($min, $max);
    if(defined($me->{ipo}->{crange})) {
	($min,$max) = @{$me->{ipo}->{crange}};
    } else {
	($min,$max) = $data->minmax;
    }
    my $g = ($min==$max)?$ data->zeroes : (($data->double - $min)/($max-$min))->clip(0,1);
    print "min=$min; max=$max, g minmax is ".$g->min.",".$g->max."\n";
    my $r = $g->sqrt;
    my $b = $g*$g;
    
    return (pdl($r,$g,$b)*255.999)->floor->mv(-1,0)->rgb_to_color;
}

 
##############################
# Plot types
#
# This probably needs a little more smarts.  
# Currently each entry is either a ppair::<foo> return or a sub that implements
# the plot type in terms of others. 

our $types = {
    lines => q{ppair::Lines},
    points => [ map { 'ppair::'.$_ } qw/Blobs Triangles Squares Crosses Xs Asterisks/ ],
    bins => sub {
	my ($me, $plot, $block, $cprops) = @_;
	my $x = $block->[0];
	my $x1 = $x->range( [[0],[-1]], [$x->dim(0)], 'e' )->average;
	my $x2 = $x->range( [[1],[0]],  [$x->dim(0)], 'e' )->average;
	my $newx = pdl($x1, $x2)->mv(-1,0)->clump(2)->sever;

	my $y = $block->[1];
	my $newy = $y->dummy(0,2)->clump(2)->sever;
	
	$plot->dataSets()->{ 1+keys(%{$plot->dataSets()}) } = 
	    ds::Pair($newx,$newy,plotType=>eval q{ppair::Lines}, @$cprops);
    },



    image => sub {
	my($me, $plot, $data, $cprops, $co) = @_;

	my ($xmin, $xmax) = $data->[0]->minmax;
	my $dx = 0.5 * ($xmax-$xmin) / ($data->[0]->dim(0) - (($data->[0]->dim(0)==1) ? 0 : 1));
	$xmin -= $dx;
	$xmax += $dx;

	my ($ymin, $ymax) = $data->[1]->minmax;
	my $dy = 0.5 * ($ymax-$ymin) / ($data->[0]->dim(1) - (($data->[1]->dim(1)==1) ? 0 : 1));
	$ymin -= $dy;
	$ymax += $dy;

	$plot->dataSets()->{ 1+keys(%{$plot->dataSets()}) } = 
	  ds::Grid($data->[2], 
		   x_bounds=>[ $xmin, $xmax ],
		   y_bounds=>[ $ymin, $ymax ],
		   plotType=>pgrid::Matrix( palette => bless({me=>$me,data=>$data,co=>$co},'PDL::Graphics::Simple::Prima::Bogus_palette')),
	  );
    },


    circles => sub {
	my($me, $plot, $data, $cprops) = @_;
	our $cstash;
	unless(defined($cstash)) {
	    my $ang = PDL->xvals(362)*3.14159/180;
	    $cstash = {};
	    $cstash->{c}   = $ang->cos;
	    $cstash->{s}   = $ang->sin;
	    $cstash->{s}->slice("361") .= $cstash->{c}->slice("361") .= PDL->pdl(1.1)->acos; # NaN
	}
	my $dr = $data->[2]->flat;
	my $dx = ($data->[0]->flat->dummy(0,1) + $dr->dummy(0,1)*$cstash->{c})->flat;
	my $dy = ($data->[1]->flat->dummy(0,1) + $dr->dummy(0,1)*$cstash->{s})->flat;
	$plot->dataSets()->{ 1+keys(%{$plot->dataSets()}) } = 
	    ds::Pair($dx, $dy, plotType=>eval q{ppair::Lines}, @$cprops);
    },


    labels => sub {
	print STDERR "\nlabels type is not yet implemented\n";
    }

};

$types->{limitbars} = sub {
    # Strategy: make T-errorbars out of the x/y/height data and generate a Line
    # plot.  The T-errorbar width is 4x the LineWidth (+/- 2x).
    my($me, $plot, $block, $cprops, $co) = @_;
    my $x = $block->[0]->flat;
    my $y = $block->[1]->flat;
    my $ylo = $block->[2]->flat;
    my $yhi = $block->[3]->flat;
    
    # Calculate T bar X ranges
    my $of = ($co->{width}||1) * 2;
    my $xp = $plot->x->reals_to_pixels($x);
    my $xlo = $plot->x->pixels_to_reals(  $xp - $of );
    my $xhi = $plot->x->pixels_to_reals(  $xp + $of );
    my $nan = PDL->new_from_specification($x->dim(0));  $nan .= asin(pdl(1.1));
    
    my $xdraw = pdl($xlo,$xhi,$x,  $x,  $xlo,$xhi,$nan)->mv(1,0)->flat; 
    my $ydraw = pdl($ylo,$ylo,$ylo,$yhi,$yhi,$yhi,$nan)->mv(1,0)->flat;
    $plot->dataSets()->{ 1+keys(%{$plot->dataSets()}) } = 
	ds::Pair($xdraw,$ydraw,plotType=>eval q{ppair::Lines}, @$cprops);
    $plot->dataSets()->{ 1+keys(%{$plot->dataSets()}) } =
	ds::Pair($x,$y,plotType=>eval ($types->{points}->[ ($me->{curvestyle}-1) %(0+@{$types->{points}}) ]), @$cprops);
};

$types->{errorbars} = sub {
    # Strategy: make T-errorbars out of the x/y/height data and generate a Line
    # plot.  The T-errorbar width is 4x the LineWidth (+/- 2x).
    my($me, $plot, $block, $cprops, $co) = @_;
    my $width = $block->[2]->flat;
    $block->[2] = $block->[1] - $width;
    $block->[3] = $block->[1] + $width;
    &{$types->{limitbars}}($me, $plot, $block, $cprops, $co);
};	  


##############################
# Plot subroutine
#
# Skeletal just now.
#
# Need to figure out how to handle overplotting.
# Also need to figure out how to control layout.
#
sub plot {
    my $me = shift;
    my $ipo = shift;

    $me->{ipo} = $ipo;

    if(defined($ipo->{legend})) {
	printf(STDERR "WARNING: Ignoring 'legend' option (Legends not yet supported by PDL::Graphics::Simple::Prima v%s)",$PDL::Graphics::Simple::VERSION);
    }
    
    # If not overplotting, erase everything in the window...
    unless($ipo->{oplot}) {
	map { $_->destroy } @{$me->{widgets}};
	$me->{curvestyle} = 0;
    }
    
    if(!defined($ipo->{multi})) {


	my $plot;

	if($ipo->{oplot} and defined($me->{last_plot})) {
	    $plot = $me->{last_plot};
	} else {
	    $plot = $me->{obj}->insert('Plot',
				      pack=>{fill=>'both',expand=>1}
	    );
	}
	push(@{$me->{widgets}}, $plot);
	$me->{last_plot} = $plot;


	## Set global plot options: titles, axis labels, and ranges.
	$plot->hide;
	$plot->title(     $ipo->{title}   )  if(defined($ipo->{title}));
	$plot->x->label(  $ipo->{xlabel}  )  if(defined($ipo->{xlabel}));
	$plot->y->label(  $ipo->{ylabel}  )  if(defined($ipo->{ylabel}));

	$plot->x->scaling(eval q{sc::Log}) if($ipo->{logaxis}=~ m/x/i);
	$plot->y->scaling(eval q{sc::Log}) if($ipo->{logaxis}=~ m/y/i);

	$plot->x->min($ipo->{xrange}->[0]) if(defined($ipo->{xrange}) and defined($ipo->{xrange}->[0]));
	$plot->x->max($ipo->{xrange}->[1]) if(defined($ipo->{xrange}) and defined($ipo->{xrange}->[1]));
	$plot->y->min($ipo->{yrange}->[0]) if(defined($ipo->{yrange}) and defined($ipo->{yrange}->[0]));
	$plot->y->max($ipo->{yrange}->[1]) if(defined($ipo->{yrange}) and defined($ipo->{yrange}->[1]));

	$plot->show;


	for my $block(@_) {
	    my $co = shift @$block;

	    # Parse out curve style (for points type selection)
	    if(defined($co->{style}) and $co->{style}) {
		$me->{curvestyle} = $co->{style};
	    } else {
		$me->{curvestyle}++;
	    }

	    my $cprops = [
		color     => eval $colors[ ($me->{curvestyle}-1) % @colors ],
		lineWidth => $co->{width} || 1
		];

	    my $type = $types->{$co->{with}};
	    if( ref($type) eq 'CODE' ) {
		&{$type}($me, $plot, $block, $cprops, $co);
	    } else {
		my $pt;
		if(ref($type) eq 'ARRAY') {
		    $pt = eval sprintf("%s",$type->[ ($me->{curvestyle}-1) % (0+@{$type}) ] );
		} elsif(!defined($type)) {
		    die "$co->{with} is not yet implemented in PDL::Graphics::Simple for Prima.\n";
		} else {
		    $pt = eval qq{$type};
		}
		
		$plot->dataSets()->{ 1+keys(%{$plot->dataSets()}) } = ds::Pair(@$block, plotType => $pt, @$cprops);
	    }
	}

	$plot->x->min($ipo->{xrange}->[0]) if(defined($ipo->{xrange}) and defined($ipo->{xrange}->[0]));
	$plot->x->max($ipo->{xrange}->[1]) if(defined($ipo->{xrange}) and defined($ipo->{xrange}->[1]));
	$plot->y->min($ipo->{yrange}->[0]) if(defined($ipo->{yrange}) and defined($ipo->{yrange}->[0]));
	$plot->y->max($ipo->{yrange}->[1]) if(defined($ipo->{yrange}) and defined($ipo->{yrange}->[1]));

	
	Prima::Timer->create(
	    onTick=>sub{$_[0]->stop; die "done with event loop\n"},
	    timeout=>50
	    )->start;
	eval { $::application->go };
	die unless $@ =~ /^done with event loop/;
	undef $@;

    } else {
	die "Multiplots not yet supported by P::G::S::Prima -- coming soon...\n";
    }   
}    
    
    
