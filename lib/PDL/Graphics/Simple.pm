=head1 NAME

PDL::Graphics::Simple - Simple backend-independent plotting for PDL

=head1 SYNOPSIS
 
 TBD

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

our $VERSION = '0.002';

##############################
# Exporting
use base 'Exporter';
our @EXPORT_OK = qw(spwin plot line point image erase);
our @EXPORT = qw(spwin);


##############################
# Configuration

# Knowledge base containing found info about each possible backend
our $mods = {};
our $mod_abbrevs = undef;
our $last_successful_type = undef;
our $global_plot = undef;

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

=back

=cut
our $new_defaults = {
    engine => '',
    size => [6,4.5,'in'],
    type => '',
    output => ''
};

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
	    attempt: for my $engine( sort {$b cmp $a} keys %$mods ) {
		print "Trying $engine ($mods->{$engine}->{engine})...";
		my $a;
		eval "\$a = $mods->{$engine}->{module}::check()";
		my $s = ($a ? "ok" : "nope");
#		chomp $@;
#		$s .= "\n ($@)\n" if ($@);
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

    my $submod= $mods->{$engine}->{module};
    my $params = { size=>$size, type=>$type, output=>$output };
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

=item key

If this is set, it should be a string containing two words: either
"top", "center", or "bottom" followed by "left", "center", or "right".
The words may be abbreviated.  

=item justify

If this is set to a nonzero value, then the plot is scaled to have the
correct aspect ratio (assuming X and Y are in the same units.

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
	    unshift(@args, xvals($args[0]), yvals($args[0]));
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
	   ((!defined($pt->{ndims}->[1])) or ($dims->dim(0) != $pt->ndims->[1]))) {
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

	##############################
	# Hammer the data to all be the same size...
	# First: accumulate dims across the data
	my @datadims = ();
	for my $arg(@args) {
	    my @argdims = $arg->dims;
	    for my $i(0..$#argdims) {
		if(  !defined($datadims[$i])  or  (  $argdims[$i] != 1   and    $datadims[$i] == 1  )   ) {
		    $datadims[$i] = $argdims[$i];
		}
		if( $argdims[$i] != 1   and   $datadims[$i] != 1   and $argdims[$i] != $datadims[$i]  ) {
		    die "Plot: argument dimensions disagree.\n";
		}
	    }
	}
	# Next: hammer the data to be exactly the same dimension.
	my $z = zeroes(@datadims);
	for my $i(0..$#args) {
	    $args[$i] = $args[$i] + zeroes($z);  
	}
	
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

 $w = new PDL::Graphics::Simple ( % opts );
 $w->line($data);

=for ref

C<line>, C<points>, C<image>, and C<imag> are convenience
interfaces.  They are exactly equivalent to C<plot> except that
they set the default "with" curve option to the appropriate
plot type.

=cut

sub _convenience_plot{
    my( $type, $me, @args ) = @_;
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

sub line   { _convenience_plot( 'line',   @_ ); }
sub points { _convenience_plot( 'points', @_ ); }
sub image  { _convenience_plot( 'image',  @_ ); }
sub imag   { _convenience_plot( 'image',  @_ ); }


##############################
# Utilities.

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
# make_mod_abbrevs - generate abbrev hash for module list.  Cheesy but fast to code.
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

This is the registration mechanism for new driver methods for C<PDL::Graphics::Simple>.
Compliant drivers should announce themselves at compile time by calling C<register>.  
At call time they should have already defined a package global hash ref, C<$mod>, containing
the following keys:

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
is working.  It registers its success or failure in the main $mods
hash, under the "ok" flag.  If there is a failure that generates an
error message, the error is logged under the "msg" flag.

C<check> accepts one parameter, "force".  If it is missing or false,
and "ok" is defined, check just echoes the prior result.  If it is
true, then check actually checks the status regardless of the "ok"
flag.

=head3 new

C<new> creates and returns an appropriate plot object, or dies on
failure.

Each C<new> method should accept the following options, defined as in 
the description for PDL::Graphics::Simple::new (above).  There is 
no need to set default values as all argument should be set to 
reasonable values by the superclass. 

Required options: C<size>, C<type>, C<output>. 

=head3 plot

C<plot> generates a plot.  It should accept a standardized collection of
options as generated by the PDL::Graphics::Simple plot method: standard
plot options as a hash ref, followed by a list of curve blocks.  Each
curve block consists of an ARRAY ref with a hash in the 0 element and
all required data in the following elements, one PDL per (ordinate/abscissa).
For 1-D plot types (like points and lines) the PDLs must be 1D.  For image
plot types the lone PDL must be 2D (monochrome) or 3D(RGB).

=cut

######################################################################
######################################################################
######################################################################
###
###
### Gnuplot interface.
###
##
#
package PDL::Graphics::Simple::Gnuplot;
use PDL::Options q/iparse/;
use File::Temp qw/tempfile/;

our $mod = {
    shortname => 'gnuplot',
    module=>'PDL::Graphics::Simple::Gnuplot',
    engine => 'PDL::Graphics::Gnuplot',
    synopsis=> 'Gnuplot 2D/3D (versatile; beautiful output)',
    pgs_version=> '0.002'
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
    output => ''
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

	# Interactive - try WXT, Aqua, X11 in that order
	if($mod->{itype}) {
	    $gpw = gpwin($mod->{itype}, @params);
	} else {
	    attempt:for my $try( 'wxt', 'aqua', 'x11' ) {
		eval { $gpw = gpwin($try, @params); };
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
	clut   => 'sepia'
    };

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

    if($ipo->{oplot}) {
	$me->{obj}->replot(@arglist);
    } else {
	$me->{obj}->plot(@arglist);
    }

    if($me->{conv_fn}) {
	print "converting $me->{conv_fn} to $me->{opt}->{output}...";
	$a = rim($me->{conv_fn});
	wim($a, $me->{opt}->{output});
	unlink($me->{conv_fn});
    }
}
	

######################################################################
######################################################################
######################################################################
###
###
### PGPLOT interface.
###
##
#

package PDL::Graphics::Simple::PGPLOT;
use File::Temp qw/tempfile/;
use PDL::Options q/iparse/;
use PDL;

our $mod = {
    shortname => 'pgplot',
    module=>'PDL::Graphics::Simple::PGPLOT',
    engine => 'PDL::Graphics::PGPLOT::Window',
    synopsis=> 'PGPLOT (old but trusted)',
    pgs_version=> '0.002'
};
PDL::Graphics::Simple::register( 'PDL::Graphics::Simple::PGPLOT' );

##########
# PDL::Graphics::Simple::PGPLOT::check
# Checker

sub check {
    my $force = shift;
    $force = 0 unless(defined($force));

    return $mod->{ok} unless( $force or !defined($mod->{ok}) );
    
    eval 'use PDL::Graphics::PGPLOT::Window;';
    if($@) {
	$mod->{ok} = 0;
	$mod->{msg} = $@;
	return 0;
    }
    
    # Module loaded OK, now try to extract valid devices from it
    my ($fh,$tf) = tempfile('pgg_pgplot_XXXX');
    close $fh;

    my $cmd = qq{|perl -e "use PGPLOT; open STDOUT,q[>$tf] || die; open STDERR,STDOUT || die; pgopen(q[?])"};
    open FOO,$cmd;
    print FOO "?\n";
    close FOO;
    open FOO,"<$tf";
    my @lines = grep /^\s+\//, (<FOO>) ;
    close FOO;
    unlink $tf;
    
    $mod->{devices} = { map { chomp; s/^\s*\///; s/\s.*//; ($_,1) } @lines };

    if( $mod->{devices}->{'XWINDOW'} ) {
	$mod->{disp_dev} = 'XWINDOW';
    } elsif($mod->{devices}->{'XSERVE'} ) {
	$mod->{disp_dev} = 'XSERVE';
    } else {
	$mod->{ok} = 0;
	return 0;
    }

    unless( $mod->{devices}->{'VCPS'} ) {
	$mod->{ok} = 0;
	return 0;
    }

    return 1;
}

##########
# PDL::Graphics::Simple::PGPLOT::new
our $new_defaults ={
    size => [8,6,'in'],
    type => '',
    output=>''
};

our $filetypes = {
    png => 'PNG',
    ps  => 'VCPS'
};

sub new {
    my $pkg = shift;
    my $opt_in = shift;
    my $opt = { iparse( $new_defaults, $opt_in ) };
    
    my $pgw;
    
    # Force a recheck on failure, in case the user fixed PGPLOT.
    # Also loads PDL::Graphics::PGPLOT::Window.
    unless(check()) {
	die "$mod->{shortname} appears nonfunctional\n" unless(check(1));
    }

    # Figure the device name and size to feed to PGPLOT.
    # size has already been regularized.
    my $conv_tempfile;
    my $dev;

    if( $opt->{type} =~ m/^i/i) {
	$dev = ( defined($opt->{output}) ? $opt->{output} : "" ) . "/" . $mod->{disp_dev};
    } else {
	my $ext;

	if( $opt->{output} =~ m/\.(\w{2,4})$/ ) {
	    $ext = $1;
	} else {
	    $ext = 'png';
	    $opt->{output} .= ".png";
	}

	our $mod;
	unless(  $filetypes->{$ext}  and  $mod->{devices}->{$filetypes->{$ext}} ) {
	    my($fh);
	    ($fh, $conv_tempfile) = tempfile('pgs_pgplot_XXXX');
	    close $fh;
	    unlink $conv_tempfile; # just to be sure...
	    $conv_tempfile .= ".ps";
	    $dev = "$conv_tempfile/VCPS";
	} else {
	    $dev = "$opt->{output}/$filetypes->{$ext}";
	}
    }

    print "dev='$dev'\n";
    print "opt->size is ".join(",",@{$opt->{size}})."\n";

    ($ENV{'PGPLOT_PS_WIDTH'}) = $opt->{size}->[0] * 1000;
    ($ENV{'PGPLOT_PS_HEIGHT'}) = $opt->{size}->[1] * 1000;


    my $creator = 'pgwin( $dev, {size=>[ $opt->{size}->[0], $opt->{size}->[1] ]} );';
    $pgw = eval $creator;
    print STDERR $@ if($@);
    
    my $me = { opt=>$opt, conv_fn=>$conv_tempfile, obj=>$pgw };
    return bless($me, 'PDL::Graphics::Simple::PGPLOT');
}

our $pgplot_methods = {
    'lines'  => 'line',
    'bins'   => 'bin',
    'points' => 'points',
    'errorbars' => sub {
	my ($me, $ipo, $data, $ppo) = @_;
	$me->{obj}->points($data->[0],$data->[1],$ppo);
	$me->{obj}->errb($data->[0],$data->[1],$data->[2]);
    },
    'limitbars'=> sub {
	my ($me, $ipo, $data, $ppo) = @_;
	# use XY absolute error form, but with X errorbars right on the point
	$me->{obj}->points($data->[0],$data->[1],$ppo);
	my $z = zeroes($data->[0]);
	$me->{obj}->errb($data->[0],$data->[1], $z, $z, -($data->[2]-$data->[1]), $data->[3]-$data->[1], $ppo);
    },
    'image'  => 'imag',
    'circles'=> sub { 
	my ($me,$ipo,$data,$ppo) = @_;
	$ppo->{filltype}='outline';
	$me->{obj}->tcircle(@$data, $ppo);
    }
};

##############################
# PDL::Graphics::Simple::PGPLOT::plot

sub plot {
    my $me = shift;
    my $ipo = shift;
    my $po = {
	title  => $ipo->{title},
	xtitle => $ipo->{xlabel},
	ytitle => $ipo->{ylabel},
	justify=> $ipo->{justify},
#	xrange => $ipo->{xrange},
#	yrange => $ipo->{yrange},
    };
    my %color_opts = ();
    if(defined($ipo->{crange})) {
	$color_opts{'MIN'} = $ipo->{crange}->[0] if(defined($ipo->{crange}->[0]));
	$color_opts{'MAX'} = $ipo->{crange}->[0] if(defined($ipo->{crange}->[1]));
    }
    
    my $more = 0;

    if($ipo->{oplot}) {
	die "oplot not yet supported";
    }

    $me->{obj}->release;
    $me->{obj}->env(@{$ipo->{xrange}}, @{$ipo->{yrange}}, $po);
    $me->{obj}->hold;

    warn "P::G::S::PGPLOT: key not implemented yet" if($ipo->{key});


    # ppo is "post-plot options", which are really a mix of plot and curve options.  
    # Currently we don't parse any plot options into it (they're handled by the "env"
    # call) but if we end up doing so, it should go here.  The linestyle and color
    # are curve options that are autoincremented each curve.
    my %ppo = ();
    my $ppo = \%ppo;
    
    $ppo->{linestyle} = 1;
    $ppo->{color}=1;
    
    while(@_) {
	my ($co, @data) = @{shift()};
	my @extra_opts = ();

	our $pgplot_methods;
	my $pgpm = $pgplot_methods->{$co->{with}};
	die "Unknown curve option 'with $co->{with}'!" unless($pgpm);


	if($pgpm eq 'imag') {
	    for my $k(keys %color_opts) {
		$po->{$k} = $color_opts{$k};
	    }

	    # Extract transform parameters from the corners of the image...
	    my $xcoords = shift(@data);
	    my $ycoords = shift(@data);

	    my $datum_pix = [0,0];
	    my $datum_sci = [$xcoords->at(0,0), $ycoords->at(0,0)];
	    
	    my $t1 = ($xcoords->slice("(-1),(0)") - $xcoords->slice("(0),(0)")) / ($xcoords->dim(0)-1);
	    my $t2 = ($xcoords->slice("(0),(-1)") - $xcoords->slice("(0),(0)")) / ($xcoords->dim(1)-1);
	    
	    my $t4 = ($ycoords->slice("(-1),(0)") - $ycoords->slice("(0),(0)")) / ($ycoords->dim(0)-1);
	    my $t5 = ($ycoords->slice("(0),(-1)") - $ycoords->slice("(0),(0)")) / ($ycoords->dim(1)-1);
	    
	    my $transform = pdl(
		$datum_sci->[0] - $t1 * $datum_pix->[0] - $t2 * $datum_pix->[1],
		$t1, $t2,
		$datum_sci->[1] - $t4 * $datum_pix->[0] - $t5 * $datum_pix->[1],
		$t4, $t5
		)->flat;

	    {   # sepia color table
		my $r = (xvals(256)/255)->sqrt;
		my $g = (xvals(256)/255);
		my $b = (xvals(256)/255)**2;
		$me->{obj}->ctab($g, $r, $g, $b);
	    }
	}

	if(ref($pgpm) eq 'CODE') {
	    &$pgpm($me, $ipo, \@data, $ppo);
	} else {
	    my $str= sprintf('$me->{obj}->%s(@data,%s);%s',$pgpm,'$ppo',"\n");
	    eval $str;
	}

	unless($pgpm eq 'imag') {
	    $ppo->{linestyle}++;
	    $ppo->{color}++;
	}

	$me->{obj}->hold;
    }
    $me->{obj}->release;
    $me->{obj}->close if($me->{opt}->{type} =~ m/^f/i);

    my $file = ( ($me->{conv_fn}) ? $me->{conv_fn} : $me->{output} );
    if(defined($file) and $file =~ m/\.ps$/) {
	print "Patching up  $file...\n";
	open FOO, "<$file";
	my @lines = <FOO>;
	my $i;
	
	for($i=0; $i<@lines; $i++) {

	    if($lines[$i] =~ m/^\%\%BoundingBox: \(atend\)/) {
		$lines[$i] = sprintf("%%%%BoundingBox: 35 35 %d %d\n", 
				     35 + 72 * $me->{opt}->{size}->[0], 
				     35 + 72 * $me->{opt}->{size}->[1]);
	    } elsif( $lines[$i] =~ m/^\%\%BoundingBox:/ ) {
		$lines[$i] = "%%\n";
	    } elsif( $lines[$i] =~ m/^\%\%PageBoundingBox:/) {
		$lines[$i] = sprintf("%%%%PageBoundingBox: 35 35 %d %d\n", 
				     35 + 72 * $me->{opt}->{size}->[0], 
				     35 + 72 * $me->{opt}->{size}->[1]);
	    }
	}
	open FOO,">$file";
	print FOO join("",@lines);
    };

    if($me->{conv_fn}) {
	$a = rim($me->{conv_fn});
	wim($a, $me->{opt}->{output});
#	unlink($me->{conv_fn});
    } 
}

1;

=head1 ARCHITECTURE

PDL::Graphics::Simple works through a central object-and-dispatch
system rather than taking full advantage of inheritance.  That is for
two reasons: (1) it makes central control mildly easier going forward
forward, since calls are dispatched through the main module; and (2)
it makes the non-object-oriented interface easier to implement since the
main interace modules are in one place and can access the global object
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
