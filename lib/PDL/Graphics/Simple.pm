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


=head1 FUNCTIONS

(More to come RSN)

=cut

package PDL::Graphics::Simple;

use strict;
use warnings;
use PDL;

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
    die("PDL::Graphics::Simple: ${name}() isn't yet implemented for ${pack}\n");
}
    
## Loop over all methods we are *supposed* to have and make a generic stub for each.
for (qw/new plot oplot replot/) {
    eval sprintf('sub %s { rsn( $_[0], "%s" }',$_, $_);
}  

##############################
# Gnuplot interface.
#

$PDL::Graphics::Simple::mods->{gnuplot} = 'PDL::Graphics::Simple::Gnuplot'; # register gnuplot
package PDL::Graphics::Simple::Gnuplot;
our @ISA = q/PDL::Graphics::Simple::Stubs/;

# Constructor
sub new {
    eval 'use PDL::Graphics::Gnuplot;';
    if($@) {
	die "PDL::Graphics::Simple: PDL::Graphics::Gnuplot doesn't load properly. Try another.\n\t$@";
    }
    my $gpw;
    eval '$gpw = gpwin();';
    if($@) {
	die "PDL::Graphics::Simple: PDL::Graphics::Gnuplot didn't construct properly.\n\t$@";
    }
    my $me = { obj=>$gpw };
    return bless($me, 'PDL::Graphics::Simple::Gnuplot');
}

# plot


# oplot -> replot

##############################
# PGPLOT interface.
$PDL::Graphics::Simple::mods->{pgplot} = 'PDL::Graphics::Simple::PGPLOT'; # register PGPLOT
package PDL::Graphics::Simple::PGPLOT;
our @ISA = q/PDL::Graphics::Simple::Stubs/;

##############################
# Prima interface.
$PDL::Graphics::Simple::mods->{prima} = 'PDL::Graphics::Simple::Prima'; # register Prima
package PDL::Graphics::Simple::Prima;
our @ISA = q/PDL::Graphics::Simple::Stubs/;


1;
