=head1 NAME

PDL::Graphics::Simple - Simple backend-independent plotting for PDL

=head1 SYNOPSIS
 
 # Simple interface - throw plots up on-screen, ASAP
 use PDL::Graphics::Simple;
 imag $a;                     # Display an image PDL
 imag $a, 0, 300;             # Display with color range
 line $rrr, $fit;             # Plot a line
 
 points $rr, $sec;            # Plot points
 hold;                        # Hold graphics so subsequent calls overplot
 line $rrr, $fit;             # Overplot a line in a contrasting color
 release;                     # Release graphics

 # Object interface - simple plotting, to file or screen
 $w = pgswin( size=>[8,4], multi=>[2,2] ); # 2x2 plot grid on an 8"x4" window
 $w = pgswin( size=>[1000,1000,'px'], output=>'plot.png' ); # output to a PNG

 $w->plot( with=>'points', $rr, $sec, with=>'line', $rrr, $fit, 
           {title=>"Points and fit", xlabel=>"Abscissa", ylabel=>"Ordinate"});

=head1 DESCRIPTION

PDL can plot through a plethora of external plotting modules.  Each
module tends to be less widely available than Perl itself, and to
require an additional step or two to install.  For simple applications 
("throw up an image on the screen", or "plot a curve") it is useful to 
have a subset of all plotting capability available in a backend-independent
layer.  PDL::Graphics::Simple provides that capability.

PDL::Graphics::Simple defines an object that represents a plotting
window/interface.  When you construct the object, you can either specify a 
backend or allow PDL::Graphics::Simple to find a backend that seems to work 
on your system.  Subsequent plotting commands are translated and passed
through to your chosen plotting module.

Only a small subset of PDL's complete graphics functionality is supported --
PDL::Graphics::Simple is intended for quick monkeying around with data 
or for demos or other applications where platform independence is more
important than configurability or plot quality.

Only 2-D plotting is supported.  For 3-D plotting, use PDL::Graphics::Gnuplot
or PDL::Graphics::Trid directly.

When plotting to a file, the file output is not guaranteed to be present
until the plot object is destroyed (e.g. by being undefed or going out of
scope).

=head1 SUPPORTED GRAPHICS ENGINES

PDL::Graphics::Simple supports the following graphics engines as
distributed.  Additional modules can be loaded dynamically;
see C<register>, below.

=over 3

=item Gnuplot (via PDL::Graphics::Gnuplot)

=item PGPLOT  (via PDL::Graphics::PGPLOT::Window)

=back

=head1 FUNCTIONS

=cut

package PDL::Graphics::Simple;

use strict;
use warnings;
use PDL;
use PDL::Options q/iparse/;
use File::Temp qw/tempfile tempdir/;
use Scalar::Util q/looks_like_number/;

our $VERSION = '0.004';

##############################
# Exporting
use base 'Exporter';
our @EXPORT_OK = qw(pgswin plot line points imag hold release erase image );
our @EXPORT = qw(pgswin line points imag hold release erase);


##############################
# Configuration

# Knowledge base containing found info about each possible backend
our $mods = {};
our $mod_abbrevs = undef;
our $last_successful_type = undef;
our $global_plot = undef;

# Attempt to load some default modules

for my $submod(qw/ PGPLOT Gnuplot PLplot Prima /) {
eval "use PDL::Graphics::Simple::$submod;";
}

=head2 show

=for usage

 PDL::Graphics::Simple::show

=for ref

C<show> lists the supported engines and a one-line synopsis of each.

=cut
sub show {
    my $format = "%-10s %-30s %-s\n";
    printf($format, "NAME","Module","(synopsis)");
    printf($format, "----","------","----------");
    for my $engine( sort keys %$mods ) {
	printf($format, $engine, $mods->{$engine}->{engine}, $mods->{$engine}->{synopsis});
    }
    print "\n";
}

##############################
# Constructor - scan through registered subclasses and generate the correct one.

=head2 new 

=for usage

 $w = new PDL::Graphics::Simple( %opts );

=for ref

c<new> is the main constructor for PDL::Graphics::Simple.  It accepts a list of options
about the type of window you want:

=over 3

=item engine

If specified, this must be one of the supported plotting engines.  You can use a module
name or the shortened name.  If you don't give one, the constructor will scan through existing
modules and pick one that seems to work.

=item size

This is a window size as an ARRAY ref containing [width, height,
units].  If no units are specified, the default should be "pixels",
using the standard conversions of 72 points per inch, 100 pixels per
inch.

=item type

This describes the kind of plot to create. It will match either /^i/i
or /^f/i.  The former is "interactive", and should result in a plot
being displayed (e.g. via X11 or the engine's default display
method). 

=item output

This should be a window number for interactive plots, or a file name
for file plots.  The default file name is "plot.png" in the current
working directory.  Individual plotting modules are meant to support
at least '.png', '.pdf', and '.ps', if necessary via format conversion.
Most other standard file types are supported but are not guaranteed to work.

=item multi

This enables plotting multiple plots on a single screen.  You feed in 
a single array ref containing (nx, ny).  Subsequent calls to plot 
send graphics to subsequent locations on the window.  The ordering 
is always horizontal first, and left-to-right, top-to-bottom.

=back

=cut
our $new_defaults = {
    engine => '',
    size => [8,6,'in'],
    type => '',
    output => '',
    multi => undef
};

sub pgswin { new('PDL::Graphics::Simple',@_); }

sub new {
    my $pkg = shift;
    my $opt_in = shift;
    $opt_in = {} unless(defined($opt_in));
    if(!(ref($opt_in))) {
	my %opt = ($opt_in, @_);
	$opt_in = \%opt;
    }

    my $opt = { iparse( $new_defaults, $opt_in ) };

    ##############################
    # Pick out a working plot engine...

    unless($opt->{engine}) {
	# find the first working subclass...
	unless($last_successful_type) {
	    attempt: for my $engine( sort keys %$mods ) {
		print "Trying $engine ($mods->{$engine}->{engine})...";
		my $a;
		my $s;
		eval "\$a = $mods->{$engine}->{module}::check()";
		if($@) {
		    chomp $@;
		    $s = "$@";
		} else {
		    $s = ($a ? "ok" : "nope");
		}
		print $s."\n";
		if($a) {
		    $last_successful_type = $engine;
		    last attempt;
		}
	    }
	      unless( $last_successful_type ) {
		  die "Sorry, all known plotting engines failed.  Install one and try again.\n";
	      }
	}
	$opt->{engine} = $last_successful_type;
    }
    
    ##############################
    # Deal with abbreviations.  
    # This can't be done at load time since the modules have to self-register then -- so 
    # we do it at run time instead.
    $mod_abbrevs = _make_abbrevs($mods) unless($mod_abbrevs);
    
    my $engine = $mod_abbrevs->{lc($opt->{engine})};
    unless(defined($engine) and defined($mods->{$engine})) {
	die "$opt->{engine} is not a known plotting engine. Use PDL::Graphics::Simple::show() for a list. ";
    }
    
    my $size = _regularize_size($opt->{size},'in');

    my $type = $opt->{type};
    my $output = $opt->{output};
    
    unless($type) {
	# Default to file if output looks like a filename; to interactive otherwise.
	$type = (  ($output =~ m/\.(\w{2,4})$/) ? 'f' : 'i'  );
    }	
    unless($type =~ m/^[fi]/i) {
	die "$type is not a known output type (must be 'file' or 'interactive')\n";
    }

    # Default to 'plot.png'  if no output is specified.
    unless($output) {
	$output = ($type eq 'f') ? "plot.png" : "";
    }
    
    # Hammer it into a '.png' if no suffix is specified
    if( $opt->{type} =~ m/^f/i   and     $output !~ m/\.(\w{2,4})$/  ) {
	$output .= ".png";
    }

    # Error-check multi
    if( defined($opt->{multi}) ) {
	if(  ref($opt->{multi}) ne 'ARRAY'  or  @{$opt->{multi}} != 2  ) {
	    die "PDL::Graphics::Simple::new: 'multi' option requires a 2-element ARRAY ref\n";
	}
	$opt->{multi}->[0] = 1  unless(  $opt->{multi}->[0]  );
	$opt->{multi}->[1] = 1  unless(  $opt->{multi}->[1]  );
    }

    my $submod= $mods->{$engine}->{module};
    my $params = { size=>$size, type=>$type, output=>$output, multi=>$opt->{multi} };
    my $obj = eval "new $mods->{$engine}->{module}(\$params)";
    my $me = { engine=>$engine, params=>$params, obj=>$obj };
    return bless($me,$pkg);

}

=head2 plot

=for usage

 $w = new PDL::Graphics::Simple ( %opts );
 $w->plot($data);

=for ref

C<plot> plots zero or more traces of data on a graph.  It accepts two kinds of
options: plot options that affect the whole plot, and curve options
that affect each curve.  The arguments are divided into "curve blocks", each
of which contains a curve options hash followed by data.  

If the last argument is a hash ref, it is always treated as plot options.
If the first and second arguments are both hash refs, then the first argument
is treated as plot options and the second as curve options for the first curve
block.

=head3 Plot options:

=over 3

=item oplot

If this is set, then the plot overplots a previous plot.

=item title

If this is set, it is a title for the plot as a whole.

=item xlabel

If this is set, it is a title for the X axis.

=item ylabel

If this is set, it is a title for the Y axis.

=item legend

If this is set, it should be a string containing two words: either
"top", "center", or "bottom" followed by "left", "center", or "right".
The words may be abbreviated.   [Note: legends are not yet implemented]

=item xrange

If this is set, it is a two-element ARRAY ref containing a range for
the X axis.  If it is clear, the engine or plot module is responsible
for setting the range.

=item yrange

If this is set, it is a two-element ARRAY ref containing a range for
the Y axis.  If it is clear, the engine or plot module is responsible
for setting the range.

=item crange 

If this is set, it is a two-element ARRAY ref containing a range for
color values, full black to full white.  If it is clear, the engine or
plot module is responsible for setting the range.

=item wedge

If this is set, then image plots get a scientific colorbar on the
right side of the plot.  (You can also say "colorbar", "colorbox", or "cb" if
you're more familiar with Gnuplot).

=item justify

If this is set to a true value, then the screen aspect ratio is adjusted
to keep the Y axis and X axis scales equal -- so circles appear circular, and
squares appear square.

=back

=head3 Curve options:

=over 3

=item with

This names the type of curve to be plotted.  See below for supported curve types.

=item legend

This gives a name for the following curve, to be placed in a master plot legend.

=back

=head3 Curve types supported

=over 3

=item points

This is a simple point plot.  It takes 1 or 2 columns of data.

=item lines

This is a simple line plot. It takes 1 or 2 columns of data.

=item bins

Stepwise line plot, with the steps centered on each X value.  1 or 2 columns.

=item errorbars

Simple points-with-errorbar plot, with centered errorbars.  It takes 2
or 3 columns, and the last column is the absolute size of the errorbar (which
is centered on the data point).

=item limitbars

Simple points-with-errorbar plot, with asymmetric errorbars.  It takes 3 or 4
columns, and the last two columns are the absolute low and high values of the 
errorbar around each point (specified relative to the origin, not relative to the
data point value).

=item circles

Plot unfilled circles.  Requires 2 or 3 columns of data; the last
column is the radius of each circle.  The circles are circular in scientific coordinates,
not necessarily in screen coordinates (unless you specify the "justify" plot option).

=item image

This is a monochrome or RGB image.  It takes a 2-D or 3-D array of values, as
(width x height x color-index).  There is no interface for pseudocolor images - 
monochrome may be greyscale or a fixed color table depending on implementation.

=back

=cut

# Plot options have a bunch of names for familiarity to different package users.  
# They're hammered into a single simplified set for transfer to the engines.

our $plot_options = new PDL::Options( {
    oplot=> 0,
    title => undef,
    xlabel=> undef,
    ylabel=> undef,
    key   => undef,
    xrange=> undef,
    yrange=> undef,
    crange=> undef,
    bounds=> undef,
    wedge => 0,
    justify=>0,
    });

$plot_options->synonyms( {
    cbrange=>'crange',
    replot=>'oplot',
    xtitle=>'xlabel',
    ytitle=>'ylabel',
    legend=>'key',
    colorbar=>'wedge',
    colorbox=>'wedge',
    cb=>'wedge'
    });

our $plot_types = {
    points    => { args=>[1,2], ndims=>[1]   },
    lines     => { args=>[1,2], ndims=>[1]   },
    bins      => { args=>[1,2], ndims=>[1]   },
    circles   => { args=>[2,3], ndims=>[1]   },
    errorbars => { args=>[2,3], ndims=>[1]   },
    limitbars => { args=>[3,4], ndims=>[1]   },
    image     => { args=>[1,3], ndims=>[2,3] },
};

our $plot_type_abbrevs = _make_abbrevs($plot_types);

sub plot {
    my $obj;
    if(UNIVERSAL::isa($_[0],"PDL::Graphics::Simple")) {
	$obj = shift;
    } else {
	$obj = $global_plot = new('PDL::Graphics::Simple');
    }

    my $curve_options = new PDL::Options( {
	with => 'lines',
	legend => undef
					  });
    $curve_options->synonyms( {
	key =>'legend',
	name=>'legend'
			      });
    $curve_options->incremental(1);


    ##############################
    # Trap some simple errors
    if($#_ == 0) {
	die "plot: requires at least one argument to plot!\n";
    }
    if($#_ == 1  and  ref($_[0]) eq 'HASH') {
	die "plot: requires at least one argument to plot, in addition to plot options\n";
    }
    
    ##############################
    # Collect plot options.  These can be in a leading or trailing
    # hash ref, with the trailing overriding the lead.  If the first
    # two elements are hash refs, then the first is plot options and
    # the second is curve options, otherwise we treat the first as curve options.
    # A curve option hash is required for every curve.
    my $po = {};
    if(ref($_[0]) eq 'HASH'   and    ref($_[1]) eq 'HASH') {
	for my $k(keys %{$_[0]}) {
	    $po->{$k} = $_[0]->{$k};
	}
	shift;
    }

    if(ref($_[$#_]) eq 'HASH') {
	for my $k(keys %{$_[$#_]}) {
	    $po->{$k} = $_[$#_]->{$k};
	};
	pop;
    }

    $po = $plot_options->options($po);
    $po->{oplot} = 1 if(defined($obj->{held}) and $obj->{held});


    ##############################
    # Check the plot options for correctness.

    ### bounds is a synonym for xrange/yrange together.
    ### (dcm likes it)
    if(defined($po->{bounds})) {
	if( !ref($po->{bounds})  or  
	    ref($po->{bounds}) ne 'ARRAY'  or
	    @{$po->{bounds}} != 2 
	    ) {
	    die "Bounds option must be a 2-element ARRAY ref containing (xrange, yrange)\n";
	}

	if( defined($po->{bounds}->[0]) ) {
	    print STDERR "WARNING: bounds overriding xrange since both were specified\n"  if(defined($po->{xrange}));
	    $po->{xrange} = $po->{bounds}->[0];
	}

	if( defined($po->{bounds}->[1]) ) {
	    print STDERR "WARNING: bounds overriding yrange since both were specified\n"  if(defined($po->{yrange}));
	    $po->{yrange} = $po->{bounds}->[1];
	}
    }

    if( defined($po->{xrange}) and (
	    !ref($po->{xrange}) or 
	    ref($po->{xrange}) ne 'ARRAY' or
	    @{$po->{xrange}} != 2 or
	    $po->{xrange}->[0] == $po->{xrange}->[1])
	) {
	die "Invalid X range (must be a 2-element ARRAY ref with differing values)\n";
    }

    if( defined($po->{yrange}) and (
	    !ref($po->{yrange}) or 
	    ref($po->{yrange}) ne 'ARRAY' or
	    @{$po->{yrange}} != 2 or
	    $po->{yrange}->[0] == $po->{yrange}->[1])
	) {
	die "Invalid Y range (must be a 2-element ARRAY ref with differing values)\n";
    }

    if( defined($po->{wedge}) ) {
	$po->{wedge} = !!$po->{wedge};
    }


    ##############################
    # Parse out curve blocks and check each one for existence.
    my @blocks = ();
    my $xminmax = [undef,undef];
    my $yminmax = [undef,undef];


    while( @_ ) {
	my $co = {};
	my @args = ();

	if (ref $_[0] eq 'HASH') {
	    $co = shift;
	} else {
	    # Attempt to parse out curve option hash entries from an inline hash.
	    # Keys must exists and not be refs and contain at least one letter.
	    while( @_  and  !ref($_[0]) and $_[0] =~ m/[a-zA-Z]/ ) {
		my $a = shift;
		my $b = shift;
		$co->{$a} = $b;
	    }
	}

	while( @_ and  (  UNIVERSAL::isa($_[0], 'PDL') or looks_like_number($_[0]) ) )  {
	    push(@args, pdl(shift));
	}

	##############################
	# Now check options
	$curve_options->options({legend=>undef});
	my %co2 = %{$curve_options->options( $co )};
	my $co2 = \%co2;

	my $ptn = $plot_type_abbrevs->{ $co2->{with} };
	unless( defined($ptn) and defined($plot_types->{$ptn}) ) {
	    die "Unknown plot type $ptn\n";
	}
	my $pt = $plot_types->{$ptn};
	$co2->{with} = $ptn;
	
	unless(@args == $pt->{args}->[0]  or  @args == $pt->{args}->[1]) {
	    die sprintf("plot style %s requires %d or %d columns; you gave %d\n",$ptn,$pt->{args}->[0],$pt->{args}->[1],0+@args);
	}
	
	# Add an index variable if needed
	if( $pt->{args}->[1] - @args == 2 ) {
	    my @dims = ($args[0]->slice(":,:")->dims)[0,1];
	    unshift(@args, xvals(@dims), yvals(@dims)); 
	}
	if( $pt->{args}->[1] - @args == 1 ) {
	    unshift(@args, xvals($args[0]) );
	}

	# Check that the PDL arguments all agree in a threading sense...
	my @dims = map { [$_->dims] } @args;
	my $dims;
	{
	    local $PDL::undefval = 1;
	    $dims = pdl(@dims);
	}
	my $dmax = $dims->mv(1,0)->maximum;
	unless( ( ($dims==1)  | ($dims==$dmax) )->all ) {
	    die "Data dimensions do not agree in plot.\n";
	}

	# Check that the number of dimensions is correct...
	if($dims->dim(0) != $pt->{ndims}->[0]  and  
	   ((!defined($pt->{ndims}->[1])) or ($dims->dim(0) != $pt->{ndims}->[1]))) {
	    die "Data dimension (".$dims->dim(0)."-D PDLs) is not correct for plot type $ptn";
	}

	# Accumulate x and y ranges...
	my @minmax;
	my $dcorner = pdl(0,0);

	# Deal with half-pixel offset at edges of images
	if($args[0]->dims > 1) {
	    my $xymat = pdl( [ ($args[0]->slice("(1),(0)")-$args[0]->slice("(0),(0)")), 
			       ($args[0]->slice("(0),(1)")-$args[0]->slice("(0),(0)")) ],
			     [ ($args[1]->slice("(1),(0)")-$args[1]->slice("(0),(0)")), 
			       ($args[1]->slice("(0),(1)")-$args[1]->slice("(0),(0)")) ]
		);
	    $dcorner = ($xymat x pdl(0.5,0.5)->slice("*1"))->slice("(0)")->abs;
	}

	@minmax = $args[0]->minmax;
	$minmax[0] -= $dcorner->at(0); 
	$minmax[1] += $dcorner->at(0);
	$xminmax->[0] = $minmax[0] if( !defined($xminmax->[0])  or  $minmax[0] < $xminmax->[0] );
	$xminmax->[1] = $minmax[1] if( !defined($xminmax->[1])  or  $minmax[1] > $xminmax->[1] );
	
	@minmax = $args[1]->minmax;
	$minmax[0] -= $dcorner->at(1); 
	$minmax[1] += $dcorner->at(1);
	$yminmax->[0] = $minmax[0] if( !defined($yminmax->[0])  or  $minmax[0] < $yminmax->[0] );
	$yminmax->[1] = $minmax[1] if( !defined($yminmax->[1])  or  $minmax[1] > $yminmax->[1] );

	# Push the curve block to the list.
	push(@blocks, [$co2, @args] );
    }

    ##############################
    # Deal with context-dependent defaults.
    $po->{xrange}->[0] = $xminmax->[0] unless(defined($po->{xrange}->[0]));
    $po->{xrange}->[1] = $xminmax->[1] unless(defined($po->{xrange}->[1]));
    $po->{yrange}->[0] = $yminmax->[0] unless(defined($po->{yrange}->[0]));
    $po->{yrange}->[1] = $yminmax->[1] unless(defined($po->{yrange}->[1]));

    ##############################
    # At long last, the parsing is over.  Dispatch the call.
    our @plot_args = ($po,@blocks);
    $obj->{obj}->plot( $po, @blocks );
}

=head2 oplot

=for usage

 $w = new PDL::Graphics::Simple ( %opts );
 $w->plot($data);
 $w->oplot($more_data);

=for ref 

C<oplot> is a convenience interface.  It is exactly
equivalent to C<plot> except it sets the plot option C<oplot>,
so that the plot will be overlain on the previous one.

=cut

sub oplot {
    my $h;

    if(ref($_[$#_]) eq 'HASH') {
	$h = $_[$#_];
    } else {
	$h = {};
	push(@_, $h);
    }
    $h->{replot} = 1;
    
    plot(@_);
}

=head2 line, points, image, imag

=for usage

 # Object-oriented convenience
 $w = new PDL::Graphics::Simple ( % opts );
 $w->line($data);

 # Very Lazy Convenience
 $a = xvals(50);
 lines $a;
 $im = sin(rvals(100,100)/3);
 imag $im;
 imag $im, 0, 1, {title=>"Bullseye?", j=>1};

=for ref

C<line>, C<points>, and C<image> are convenience
interfaces.  They are exactly equivalent to C<plot> except that
they set the default "with" curve option to the appropriate
plot type.

C<imag> is even more DWIMMy for PGPLOT users or PDL Book readers:
it accepts up to three non-hash arguments at the start of the
argument list.  The second and third are taken to be values for 
the C<crange> plot option.

=cut

sub _convenience_plot{
    my $type = shift;
    my $me;
    if( UNIVERSAL::isa($_[0], 'PDL::Graphics::Simple') ) {
	$me = shift;
    } else {
	$me = _global_or_new();
    }

    my @args = @_;

    die "Not enough args to PDL::Graphics::Simple::$type()\n" if( @args < 1 );
    if( ref($args[0]) eq 'HASH' ) {
	if( ref($args[1]) eq 'HASH' ) {
	    $args[1]->{with} = $type;
	} else {
	    $args[0]->{with} = $type;
	}
    } else {
	unshift(@args, 'with', $type);
    }
    plot( $me, @args );
}

sub line    { _convenience_plot( 'line',   @_ ); }
*PDL::line   = \&line;

sub lines   { _convenience_plot( 'line',   @_ ); }  
*PDL::lines  = \&lines;

sub points  { _convenience_plot( 'points', @_ ); }  
*PDL::points = \&points;

sub image   { _convenience_plot( 'image', @_ ); }   #  Don't PDL-class image since it's so different from imag.

sub imag   { 
    my $me;
    if( UNIVERSAL::isa($_[0], 'PDL::Graphics::Simple') ) {
	$me = shift;
    } else {
	$me = _global_or_new();
    }
    my $data = shift;
    my $crange = [];
    unless(ref($_[0]) eq 'HASH') {
	$crange->[0] = shift;
	
	unless(ref($_[0]) eq 'HASH') {
	    $crange->[1] = shift;
	}
    }
    
    # Try to put the crange into the plot options, if they are present
    unless( ref($_[$#_]) eq 'HASH' ) {
	push(@_, {} );
    }
    $_[$#_]->{crange} = $crange;

    _convenience_plot( 'image',  $data, @_ );
}
*PDL::imag = \&imag;

=head2 erase

=for usage 

 use PDL::Graphics::Simple qw/erase hold release/;
 line xvals(10), xvals(10)**2 ;
 sleep 5;
 erase;

=for ref 

C<erase> removes a global plot window.  It should not be called as a method.
To remove a plot window contained in a variable, undefine it.

=cut

our $global_object;

sub erase {
    my $me = shift;
    if(defined($me)) {
	die "PDL::Graphics::Simple::erase: no arguments, please.";
    }
    if(defined($global_object)) {
	undef $global_object;
    }
}

=head2 hold

=for usage

 use PDL::Graphics::Simple;
 line xvals(10);
 hold;
 line xvals(10)**0.5;

=for ref

Causes subsequent plots to be overplotted on any existing one.  Called
as a function with no arguments, C<hold> applies to the global object.
Called as an object method, it applies to the object.

=cut

sub hold {
    my $me = shift;
    if(defined($me) and UNIVERSAL::isa($me,"PDL::Graphics::Simple")) {
	$me->{held} =1;
    } elsif(defined($global_object)) {
	$global_object->{held}=1;
    } else {
	die "Can't hold a nonexistent window!\n";
    }
}

=head2 release

=for usage

 use PDL::Graphics::Simple;
 line xvals(10);
 hold;
 line xvals(10)**0.5;
 release;
 line xvals(10)**0.5;

=for ref

Releases a hold placed by C<hold>.

=cut

sub release {
    my $me = shift;
    if(defined($me) and UNIVERSAL::isa($me,"PDL::Graphics::Simple")) {
	$me->{held} = 0;
    } elsif(defined($global_object)) {
	$global_object->{held} = 0;
    } else {
	die "Can't release a nonexistent window!\n";
    }
}

##############################
# Utilities.


sub _global_or_new {
    unless(defined($global_object)) {
	$global_object = pgswin();
    }
    return $global_object;
}


### Units table - cheesy but also horrible.
our $units = {
    'inch'=>1,
    'inc'=>1,
    'in' =>1,
    'i' => 1,
    'char'=>16,
    'cha'=>16,
    'ch'=>16,
    'c'=>16,
    'points'=>72,
    'point'=>72,
    'poin'=>72,
    'poi'=>72,
    'po'=>72,
    'pt'=>72,
    'px'=>100,
    'pixels'=>100,
    'pixel'=>100,
    'pixe'=>100,
    'pix'=>100,
    'pi'=>100,
    'p'=>100,
    'mm' => 25.4,
    'cm' => 2.54
};

### regularize_size -- handle the various cases for the size option to new.
sub _regularize_size {
    my $size = shift;
    my $unit = shift;
    
    $unit =~ tr/A-Z/a-z/;
    die "size specifier unit '$unit' is unrecognized\n" unless($units->{$unit});

    unless(ref($size)) {
	$size = [ $size, $size, 'in' ];
    } elsif(ref($size) ne 'ARRAY') {
	die "size option requires an ARRAY ref or scalar\n";
    } 
    die "size array must have at least one element\n" unless(@{$size});
    $size->[1] = $size->[0]     if(@{$size}==1);
    $size->[2] = 'in'           if(@{$size}==2);
    die "size array can have at most three elements\n" if(@{$size}>3);
    die "size array unit '$unit' is unrecognized\n" unless($units->{$unit});
    die "new: size must be nonnegative\n" unless( $size->[0] > 0   and   $size->[1] > 0 );

    my $ret = [];
    $ret->[0] = $size->[0] / $units->{$size->[2]} * $units->{$unit};
    $ret->[1] = $size->[1] / $units->{$size->[2]} * $units->{$unit};
    $ret->[2] = $unit;
    return $ret;
}

##########
# make_abbrevs - generate abbrev hash for module list.  Cheesy but fast to code.
sub _make_abbrevs {
    my $hash = shift;
    my $abbrevs = {};
    my %ab = ();
    for my $k(keys %$hash) {
	my $s = $k;
	while(length($s)) {
	    push(@{$ab{$s}},$k);
	    chop $s;
	}
    }
    for my $k(keys %ab) {
	$abbrevs->{$k} = $ab{$k}->[0] if( @{$ab{$k}} == 1);
    }
    return $abbrevs;
}

=head2 register

=for usage

 PDL::Graphics::Simple::register( $module_name );

=for ref

This is the registration mechanism for new driver methods for
C<PDL::Graphics::Simple>.  Compliant drivers should announce
themselves at compile time by calling C<register>.  When they do that,
they should have already defined a package global hash ref, C<$mod>,
containing the following keys:

=over

=item shortname

This is the short name of the engine, by which users refer to it colloquially.

=item module

This is the fully qualified package name of the module itself.

=item engine

This is the fully qualified package name of the Perl API for the graphics engine.

=item synopsis

This is a brief string describing the backend

=item pgs_version 

This is a one-period version number of PDL::Graphics::Simple against which
the module has been tested.  A warning will be thrown if the version isn't the
same as C<$PDL::Graphics::Simple::VERSION>.

=back

=cut
sub register {
    my $module = shift;
    
    my $modname = "\$${module}::mod";
    die "PDL::Graphics::Simple::register: tried to register $module \n\t...but $modname wasn't defined.\n"
	unless (eval qq{defined($modname) and ref($modname) eq 'HASH';});

    my $mod = eval $modname;

    for(qw/shortname module engine synopsis pgs_version/) {
	die "PDL::Graphics::Simple::register: $modname looks fishy; I give up\n" 
	    unless( defined($mod->{$_}));
    }

    warn "PDL::Graphics::Simple::register: $module is out of date - winging it"
	unless($mod->{pgs_version} eq $VERSION);

    $mods->{$mod->{shortname}} = $mod;
}


##############################
# Methods.
# 
# We subclass each plotting backend, and as a belt-and-suspenders check
# we inherit stub functions that throw errors

# The "new" method should check that its module loads properly and the constructor
# succeeded, and should die if it failed.


=head1 Internals

PDL::Graphics::Simple defines several subclasses that implement the
individual interfaces.  The subclasses are very simple and inherit
only a collection of stubroutines that die with a "not implemented"
message -- i.e. they are expected to contain a minimum set of methods
on their own.

Argument parsing and defaults are handled by the main
PDL::Graphics::Simple class; actual plot commands are dispatched in a
regularized form to the appropriate subclasses.  The subclasses are
responsible for converting the regularized parameters to plot calls 
in the form expected by their corresponding plot modules.

=head2 Interface subclass methods

Each interface module supports the following methods:

=cut

# Note that these are =head3; that means they won't be indexed by PDL::Doc, 
# which is a Good Thing as they are internal routines.

=head3 check 

C<check> attempts to load the relevant engine module and test that it
is working.  In addition to returning a boolean value indicating
success if true, it registers its success or failure in
the main $mods hash, under the "ok" flag.  If there is a failure that
generates an error message, the error is logged under the "msg" flag.

C<check> accepts one parameter, "force".  If it is missing or false,
and "ok" is defined, check just echoes the prior result.  If it is
true, then check actually checks the status regardless of the "ok"
flag.

=head3 new

C<new> creates and returns an appropriate plot object, or dies on
failure.

Each C<new> method should accept the following options, defined as in 
the description for PDL::Graphics::Simple::new (above).  There is 
no need to set default values as all arguments should be set to 
reasonable values by the superclass.

For file output, the method should autodetect file type by dot-suffix.
At least ".png" and ".ps" should be supported.

Required options: C<size>, C<type>, C<output>, C<multi>.

=head3 plot

C<plot> generates a plot.  It should accept a standardized collection
of options as generated by the PDL::Graphics::Simple plot method:
standard plot options as a hash ref, followed by a list of curve
blocks.  It should render either a full-sized plot that fills the plot
window or, if the object C<multi> option was set on construction, the
current subwindow.  For interactive plot types it should act as an
atomic plot operation, displaying the complete plot.  For file plot
types the atomicity is not well defined, since multiplot grids may
be problematic, but the plot should be closed as soon as practical.

The plot options hash contains the plot options listed under C<plot>,
above, plus one additional flag - C<oplot> - that indicates the new
data is to be overplotted on top of whatever is already present in the
plotting window.  All options are present in the hash. The C<title>,
C<xlabel>, C<ylabel>, and C<legend> options default to undef, which
indicates the corresponding plot feature should not be rendered.  The
C<oplot>, C<xrange>, C<yrange>, C<crange>, C<wedge>, and C<justify>
parameters are always both present and defined.

If the C<oplot> plot option is set, then the plot should be overlain on 
a previous plot - otherwise the module should display a fresh plot.

Each curve block consists of an ARRAY ref with a hash in the 0 element
and all required data in the following elements, one PDL per
(ordinate/abscissa).  For 1-D plot types (like points and lines) the
PDLs must be 1D.  For image plot types the lone PDL must be 2D
(monochrome) or 3D(RGB).

The hash in the curve block contains the curve options for that
particular curve.  They are all set to have reasonable default values.
The values passed in are C<with> and C<legend>.  If the C<legend>
option is undefined, then the curve should not be placed into a plot
legend (if present).  The C<with> option will be one of C<points>,
C<lines>, C<bins>, C<errorbars>, C<limitbars>, C<circles>, or
C<image>.

=cut
	

1;

=head1 ARCHITECTURE

PDL::Graphics::Simple works through a central object-and-dispatch
system rather than taking full advantage of inheritance.  That is for
two reasons: (1) it makes central control mildly easier going forward,
since calls are dispatched through the main module; and (2) it makes
the non-object-oriented interface easier to implement since the main
interface modules are in one place and can access the global object
easily.

=head1 REPOSITORY

L<https:/github.com/drzowie/PDL-Graphics-Simple>

=head1 AUTHOR

Craig DeForest, C<< <craig@deforest.org> >>


=head1 LICENSE AND COPYRIGHT

Copyright 2012 Craig DeForest

This program is free software; you can redistribute it and/or modify
it under the terms of either: the Gnu General Public License v2 as
published by the Free Software Foundation; or the Perl Artistic
License included with the Perl language.

see http://dev.perl.org/licenses/ for more information.

=cut
