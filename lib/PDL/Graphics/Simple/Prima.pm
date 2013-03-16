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
package PDL::Graphics::Simple::Prima;

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
    
    my $pw;

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


# [Need to implement colors too.]
our @colors = ();   # What goes in here?  I don't know yet.
 
##############################
# Plot types
#
# This probably needs a little more smarts.  
# Currently each entry is either a ppair::<foo> return or a sub that implements
# the plot type in terms of others. 
our $types = {
    lines => ppair::Lines,
    points => [ map { 'ppair::'.$_ } qw/Blobs Triangles Squares Crosses Xs Asterisks/ ],
    bins => undef,
    errorbars => undef,
    limitbars => undef,
    image => undef,
    circles => undef,
    labels => undef
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
    if(defined($ipo->{legend})) {
	printf(STDERR "WARNING: Ignoring 'legend' option (Legends not yet supported by PDL::Graphics::Simple::Prima v%s)",$PDL::Graphics::Simple::VERSION);
    }
    
    # If not overplotting, erase everything in the window...
    unless($ipo->{oplot}) {
	map { $_->destroy } @{$me->{widgets}};
	$me->{curvestyle} = 0;
    }
    
    if(!defined($ipo->{multi})) {

	my @plot_args = ();

	my $plot = $me->{obj}->insert('Plot',
				      pack=>{fill=>'both',expand=>1}
	    );
	push(@{$me->{widgets}}, $plot);

	for my $block(@_) {
	    my $co = shift @$block;

	    # Get data and generate sequence if necessary
	    if(@$block < 2) {
		unshift(@$block, sequence($block->[0]));
	    }

	    # Parse out curve style (for points type selection)
	    if(defined($co->{style}) and $co->{style}) {
		$me->{curvestyle} = $co->{style};
	    } else {
		$me->{curvestyle}++;
	    }
	    
	    my %plot_args;

	    my $type = $types->{$co->{with}};
	    if( ref($type) eq 'CODE' ) {
		&{$type}($me, $block);
	    } else {
		my $pt;
		if(ref($type) eq 'ARRAY') {
		    $pt = eval sprintf("%s",$type->[ ($me->{curvestyle}-1) % (0+@{$type}) ] );
		} else {

		    $pt = eval qq{$type};
		}

		$plot->dataSets()->{ 1+keys(%{$plot->dataSets()}) } = ds::Pair(@$block, plotType => $pt);
	    }
	}


	## Set global plot options like title
	$plot->title($ipo->{title})     if(defined($ipo->{title}));
	$plot->x->label($ipo->{xlabel}) if(defined($ipo->{xlabel}));
	$plot->y->label($ipo->{ylabel}) if(defined($ipo->{ylabel}));

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
    
    
