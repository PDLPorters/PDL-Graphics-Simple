#!perl

BEGIN {
    our $tests_per_engine = 11;
    our @engines = qw/pgplot gnuplot/;
}
use Test::More tests=> (3 + (@engines)*($tests_per_engine));

use File::Temp q/tempfile/;
use PDL;

##############################
# Module loads properly
eval "use PDL::Graphics::Simple;";
ok(!$@);

eval "PDL::Graphics::Simple::show();";
ok(!$@);

*mods = \$PDL::Graphics::Simple::mods;
ok( (  defined($mods) and ref $mods eq 'HASH'  ) ,
    "module registration hash exists");

for $engine(@engines) {

    ok( (  $mods->{$engine} and ref($mods->{$engine}) eq 'HASH' and ($module = $mods->{$engine}->{module}) ),
	"there is a modules entry for $engine ($module)" );

  SKIP: {

      eval qq{\$check_ok = ${module}::check(1)};
      ok(!$@, "${module}::check() ran OK");

      unless($check_ok) {
	  skip "Skipping tests for engine $engine (not working)", 10;
      }

      eval { $w = new PDL::Graphics::Simple(engine=>$engine) };
      ok( ( !$@ and ref($w) eq 'PDL::Graphics::Simple' ), "contructor for $engine worked OK");


##############################
# Simple line & bin plot
      eval { $w->plot(with=>'line', xvals(10), xvals(10)->sqrt * sqrt(10), 
		      with=>'bins', sin(xvals(10))*10,
		 {title=>"PDL Simple Graphics: $engine engine, line & bin plots"}),

      };
      ok(!$@, "plot succeeeded\n");
      print STDERR <<"FOO";
Testing $engine engine: You should see a superposed line plot and bin
plot, with x range from 0 to 9 and yrange from 0 to 9. The two plots
should have different line styles.  OK? (Y/n)
FOO
      $a = <STDIN>;
      ok( $a !~ m/^n/i, "line plot looks ok" );


##############################
# Image & circles plot
      eval { $w->plot(with=>'image', rvals(11,11), 
		      with=>'circle', xvals(15), xvals(15)*1.5, sin(xvals(15))**2 * 4,
		      {title=>"PDL Simple Graphics: $engine, image & circle plots (not justified)"}
		 );
      };
      ok(!$@, "plot succeeded\n");
      print STDERR <<"FOO";
Testing $engine engine: You should see a radial 11x11 "target" image
and some superimposed "circles".  Since the plot is not justified, the
pixels in the target image should be oblong and the "circles" should
be ellipses.  OK? (Y/n) 
FOO
      $a = <STDIN>; 
      ok( $a !~ m/^n/i,
	  "image and circles plot looks ok");

##############################
# Image & circles plot (justified)
      eval { $w->plot(with=>'image', rvals(11,11), 
		      with=>'circle', xvals(15), xvals(15)*1.5, sin(xvals(15))**2 * 4,
		      {title=>"PDL Simple Graphics: $engine, image & circle plots (justified)", j=>1}
		 );
      };
      ok(!$@, "justified image and circles plot succeeded"); print($@) if($@);
      print STDERR <<"FOO";
Testing $engine engine: You should see the same plot as before, but
justified.  superimposed "circles".  Since the plot is justified,
the pixels in the target image should be square and the "circles" should
really be circles.  OK? (Y/n) 
FOO
      $a = <STDIN>; 
      ok( $a !~ m/^n/i,
	  "justified image and circles plot looks ok");

##############################
# Error bars plot
      eval { $w->plot( with=>'errorbars', xvals(37)*72/36, (xvals(37)/3)**2, xvals(37),
		       with=>'limitbars', sin(xvals(90)*4*3.14159/90)*30 + 72, xvals(90)/2, ones(90)*110,
		       {title=>"PDL Simple Graphics: $engine, error bars (rel.) & limit bars (abs.)"}
		 ); };
      ok(!$@, "errorbar plot succeeded"); print($@) if($@);
      
      print STDERR <<"FOO";
Testing $engine engine: You should see error bars (symmetric relative to each
plotted point) and limit bars (asymmetric about each plotted point).
OK? (Y/n)
FOO
      $a = <STDIN>; 
      ok( $a !~ m/^n/i,
	  "errorbars / limitbars OK");


    }
}





