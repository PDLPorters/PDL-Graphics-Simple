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

Only a small subset of PDL's graphics functionality is supported --
PDL::Graphics::Simple is intended for quick monkeying around with data 
or for demos or other applications where platform independence is more
important than configurability, plot quality, or speed.

Only 2-D plotting is supported.  For 3-D plotting, use PDL::Graphics::Gnuplot
or PDL::Graphics::Trid directly.

=head1 SUPPORTED BACKENDS

PDL::Graphics::Simple supports:

=over 3

=item Gnuplot (via PDL::Graphics::Gnuplot)

=item PGPLOT  (via PDL::Graphics::PGPLOT::Window)

=item PLplot (via PDL::Graphics::PLplot)

=item Prima (via PDL::Graphics::Prima).

=back

=head1 FUNCTIONS

(More to come RSN)

=cut

package PDL::Graphics::Simple;

use strict;
use warnings;
use PDL;
use PDL::Options q/iparse/;
use File::Temp qw/tempfile tempdir/;

our $VERSION = '0.1';

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
    printf($format, "----","------","----------\n");
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

This describes the kind of plot to create. The default is
"interactive" if neither type nor output are specified, and should
result in a plot being displayed (e.g. via X11 or the engine's default
display method). If the output is specified and appears to be a qualified
graphics filename (i.e. has a dot-extension with 2-4 characters at the end)
the default is 'f'.

Accepted values are "interactive" and "file", which can be abbreviated
to 'i' and 'f'.

=item output

This should be a window number for interactive plots, or a file name
for file plots.  The default file name is "plot.png" in the current
working directory.  Individual plotting modules are meant to support
at least '.png', '.pdf', and '.ps', if necessary via format conversion.
Most other standard file types are supported but are not guaranteed to work.

=back

=cut
our $dstr = '';
our $new_defaults = {
    engine => '',
    size => [6,4.5,'in'],
    type => $dstr,
    output => $dstr
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
	    attempt: for my $engine( sort keys %$mods ) {
		print "Trying $engine ($mods->{$engine}->{engine})...";
		my $a;
		eval "\$a = $mods->{$engine}->{module}::check()";
		my $s = ($a ? "ok" : "nope");
		$s .= " ($@)" if ($@);
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

    unless($output) {
	$output = ($type eq 'f') ? "plot.png" : "";
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

TBD

=item xrange

If this is set, it is a two-element ARRAY ref containing a range for the X axis.

=item yrange

If this is set, it is a two-element ARRAY ref containing a range for the Y axis.

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

=item lines

This is a simple line plot. It takes 1 or 2 columns of data.

=item points

This is a simple point plot.  It takes 1 or 2 columns of data.

=item image

This is a monochrome or RGB image.  It takes a 2-D or 3-D array of values, as
(width x height x color-index).

=back

=cut

# Plot options have a bunch of names for familiarity to different package users.  
# They're hammered into the gnuplot-like names.

our $plot_options = new PDL::Options( {
    oplot=> 0,
    title => undef,
    xlabel=> undef,
    ylabel=> undef,
    key   => undef,
    xrange=> undef,
    yrange=> undef
    });
$plot_options->synonyms( {
    replot=>'oplot',
    xtitle=>'xlabel',
    ytitle=>'ylabel',
    legend=>'key'
    });

    
our $plot_types = {
    lines  => { args=>[1,2], ndims=>[1]   },
    points => { args=>[1,2], ndims=>[1]   },
    image  => { args=>[1,3], ndims=>[2,3] },
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
    # Parse out curve blocks and check each one for existence.
    my @blocks = ();
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

	while( @_ and  ( !ref($_[0])  or  UNIVERSAL::isa($_[0], 'PDL') ) ) {
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
	
	# Add an index if needed
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

	# Push the curve block to the list.
	push(@blocks, [$co2, @args] );
    }

    ##############################
    # At long last, the parsing is over.  Dispatch the call.
    our @plot_args = ($po,@blocks);
    $obj->{obj}->plot( $po, @blocks );
}

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

sub line {
}

sub points {
}

sub image {
}

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

##############################
# Methods.
# 
# We subclass each plotting backend, and as a belt-and-suspenders check
# we inherit stub functions that throw errors

# The "new" method should check that its module loads properly and the constructor
# succeeded, and should die if it failed.


##############################
# Stubs subclass -- die if anyone tries anything funny.
package PDL::Graphics::Simple::Stubs;
## Generic stub
sub rsn {
    my($pack, $name) = @_;
    $pack = ref $pack if(ref $pack);
    die("PDL::Graphics::Simple: ${name}() isn't yet implemented for ${pack}.\n");
}

############################## 
## Loop over all methods we are *supposed* to have and make a generic stub for each.
for my $stub(qw/check new plot oplot replot/) {
    eval sprintf('sub %s { rsn( $_[0], "%s" )}',$stub, $stub);
}  

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

Notes:

=over 3

=item replot/oplot

One of the plot options should cause overplotting rather than direct clear plotting.

=item plot types

Standard plot types (lines, points, tbd) are generated as individual curves;
the "lines" convenience routine in the parent module just invokes plot.

=item argument parsing

irregularities in arguments are parsed by the parent module.

=back

=head3 

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

our @ISA = q/PDL::Graphics::Simple::Stubs/;

$PDL::Graphics::Simple::mods->{gnuplot} = {
    module => 'PDL::Graphics::Simple::Gnuplot',
    engine => 'PDL::Graphics::Gnuplot',
    synopsis=> 'Gnuplot 2D/3D (versatile; beautiful output)'
};

##########
# PDL::Graphics::Simple::Gnuplot::check
# Checker
sub check {
    my $force = shift;
    $force = 0 unless(defined($force));

    my $mod = $PDL::Graphics::Simple::mods->{gnuplot};

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
our $dstr = '';
our $new_defaults = {
    size => [6,4.5,'in'],
    type => $dstr,
    output => $dstr
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
	check(1);
    }

    my $mod = $PDL::Graphics::Simple::mods->{gnuplot};
    
    # Generate the @params array to feed to gnuplot
    my @params;
    push( @params, "size" => $opt->{size} );
    
    # tempfile gets set if we need to write to a temporary file for image conversion
    my $conv_tempfile = '';

    # Do different things for interactive and file types
    if($opt->{type} =~ m/^[iI]/) {
	push(@params, "output"=>$opt->{output});

	# Interactive - try WXT, Aqua, X11 in that order
	if($mod->{itype}) {
	    $gpw = gpwin($mod->{itype}, @params);
	} else {
	    for my $try( 'wxt', 'aqua', 'x11' ) {
		eval { $gpw = gpwin($try, @params); };
		last if($gpw);
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
	    $opt->{output} .= ".png";
	    $ext = "png";
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
	    ($fh,$conv_tempfile) = tempfile();
	    close $fh;
	}
	push( @params, "output" => ($conv_tempfile || $opt->{output}) );
	$gpw = gpwin( $conv_tempfile ? 'png' : $ext,  @params );
    }
    my $me = { opt => $opt, conv_fn => $conv_tempfile, obj=>$gpw };
    return bless($me, 'PDL::Graphics::Simple::Gnuplot');
}

# plot

sub plot {
    my $me = shift;

    my $ipo = shift;
    my $po = {
	title  => $ipo->{title},
	xlab   => $ipo->{xlabel},
	ylab   => $ipo->{ylabel},
	key    => $ipo->{key},
	xrange => $ipo->{xrange},
	yrange => $ipo->{yrange}
    };

    if($ipo->{oplot}) {
	$me->{obj}->replot($po, map { (@$_) } @_);
    } else {
	$me->{obj}->plot($po, map { (@$_) } @_);
    }
}
	

##############################
# PGPLOT interface.
$PDL::Graphics::Simple::mods->{pgplot} = { # register PGPLOT
    module => 'PDL::Graphics::Simple::PGPLOT',
    engine => 'PDL::Graphics::PGPLOT::Window',
    synopsis => 'PGPLOT (old but trusted)'
};
package PDL::Graphics::Simple::PGPLOT;
our @ISA = q/PDL::Graphics::Simple::Stubs/;

##############################
# Prima interface.
$PDL::Graphics::Simple::mods->{prima} = { # register Prima
    module  => 'PDL::Graphics::Simple::Prima',
    engine => 'PDL::Graphics::Prima',
    synopsis => 'Prima (under devel.; fast, interactive.)'
}; 
package PDL::Graphics::Simple::Prima;
our @ISA = q/PDL::Graphics::Simple::Stubs/;


1;

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
