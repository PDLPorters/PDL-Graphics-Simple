#!perl

use Test::More tests=>2;

use File::Temp q/tempfile/;
use PDL;

##############################
# Module loads
eval "use PDL::Graphics::Simple;";
ok(!$@);

eval "PDL::Graphics::Simple::show();";
ok(!$@);





