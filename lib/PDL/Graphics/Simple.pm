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
use PDL::Lite;
use PDL::Options q/parse/;
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
our $last_successful_type = undef;

=head2 show

=for usage

 PDL::Graphics::Simple::show

=for ref

C<list> lists the supported engines.

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

new is the main constructor for PDL::Graphics::Simple.  It accepts a list of options
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

This should describe the kind of plot to create. The default is
"interactive" if neither type nor output are specified, and should
result in a plot being displayed (e.g. via X11 or the engine's default
display method).

Accepted values are "interactive" and "file".

=item output

This should be a window number for interactive plots, or a file name
for file plots.  The default file name is "plot.png" in the current
working directory.  The output should be autodetected and given the
correct output style for common extensions.  At least ".png", ".pdf",
".ps", and ".svg" should be supported.  It is acceptable to convert
the file after generation by the back-end engine.  If "output" is
defined and non-numeric, then "type" should default to "file".

=back


=cut
sub new {
}

sub plot {
}

sub oplot {
}

sub line {
}

sub points {
}

sub image {
}

##############################
# Utilities.
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
sub regularize_size {
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
message. 

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

Each c<new> method should accept the following options, defined by the
PDL::Graphics::Simple::new method (above).

=over 3

=item size

=item type

=item output

=back

=head3 plot

C<plot> generates a plot.  It should accept a standardized collection of
options as generated by the PDL::Graphics::Simple plot method: standard
plot options as a hash ref, followed by curve blocks consisting of 
a curve hash ref defining the curve, followed by the exact required 
number of PDLs for that curve type.  

Details TBD.

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
use PDL::Options q/parse/;
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
our $dstr = '%%default%%';
our $new_defaults = {
    size => [6,4.5,'in'],
    type => $dstr,
    output => $dstr
};


sub new {
    my $class = shift;
    my $opt_in = shift;
    $opt_in = {} unless(defined($opt_in));
    my $opt = { parse( $new_defaults, $opt_in ) };
    my $gpw;

    # Force a recheck on failure, in case the user fixed gnuplot.
    # Also loads PDL::Graphics::Gnuplot.
    unless(check()) {
	check(1);
    }

    my $mod = $PDL::Graphics::Simple::mods->{gnuplot};
    
    if($opt->{type} !~ m/^[ifIF]/ and $opt->{type} ne $dstr ) {
	die "PDL::Graphics::Simple::new: unknown window type '$opt->{type}'\n\t(must be 'interactive' or 'file')\n";	}

    if($opt->{type} eq $dstr) {
	$opt->{type} = (   ( $opt->{output} eq $dstr) ? 'i' : 'f'   );
    }


    # Generate the @params array to feed to gnuplot
    my @params;
    my $size = PDL::Graphics::Simple::regularize_size($opt->{size},'in');
    push( @params, "size" => $size );
    
    
    # tempfile gets set if we need to write to a temporary file for image conversion
    my $conv_tempfile = '';

    # Do different things for interactive and file types
    if($opt->{type} =~ m/^[iI]/) {
	push(@params, "output"=>$opt->{output}) unless($opt->{output} eq $dstr);

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
	if($opt->{output} eq $dstr) {
	    $opt->{output} = "plot.png";
	}


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

# oplot -> replot

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
