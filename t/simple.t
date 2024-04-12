use strict;
use warnings;
use PDL::Graphics::Simple;
use Test::More;
use PDL;
use PDL::Constants qw(PI);

my $tests_per_engine = 17;
my @engines = $ENV{PDL_SIMPLE_ENGINE} || qw/plplot gnuplot pgplot prima/;
my $smoker = ($ENV{'PERL_MM_USE_DEFAULT'} or $ENV{'AUTOMATED_TESTING'});
$ENV{PGPLOT_DEV} ||= '/NULL' if $smoker;

sub ask_yn {
    my ($msg, $label) = @_;
    return pass $label if $smoker;
    print STDERR qq{\n\n$msg  OK? (Y/n) > };
    my $a = <STDIN>;
    unlike($a, qr/n/i, $label);
}

##############################
# Try the simple engine and convenience interfaces...

{
my @new = PDL::Graphics::Simple::_translate_new();
# ignore $engine
is_deeply $new[1], {
  'multi' => undef, 'output' => '', 'size' => [ 8, 6, 'in' ], 'type' => 'i'
} or diag explain \@new;
}

{
my $a = xvals(50); my $sin = sin($a/3);
my $type = 'line';
my $me = PDL::Graphics::Simple::_invocant_or_global();
my @args = PDL::Graphics::Simple::_translate_plot(@$me{qw(held keys)}, PDL::Graphics::Simple::_translate_convenience($type, $a, $sin));
is_deeply \@args, [
  [ 'line 1' ],
  {
    'bounds' => undef, 'crange' => undef,
    'justify' => 0, 'legend' => undef,
    'logaxis' => '', 'oplot' => 0,
    'title' => undef, 'wedge' => '',
    'xlabel' => undef, 'ylabel' => undef,
    'xrange' => [ 0, 49 ],
    'yrange' => [ '-0.999990206550703', '0.995407957751765' ]
  },
  [
    { 'key' => undef, 'style' => undef, 'width' => undef, 'with' => 'lines' },
    $a, $sin,
  ]
];
}
eval { $a = xvals(50); lines $a sin($a/3) };
plan skip_all => 'No plotting engines installed' if $@ =~ /Sorry, all known/;
is($@, '', "simple lines plot succeeded");
ok( defined($PDL::Graphics::Simple::global_object), "Global convenience object got spontaneously set" );
ask_yn q{  test>  $a = xvals(50); lines $a sin($a/3);
You should see a sine wave...}, "convenience plot OK";

eval { erase };
is($@, '', 'erase worked');
ok(!defined($PDL::Graphics::Simple::global_object), 'erase erased the global object');

eval { PDL::Graphics::Simple::show() };
is($@, '');

my $mods = do { no warnings 'once'; $PDL::Graphics::Simple::mods };
ok( (  defined($mods) and ref $mods eq 'HASH'  ) ,
    "module registration hash exists");

# line & bin
my $x10 = xvals(10);
my $x10sqrt = $x10->sqrt * sqrt(10);
my $sin10 = sin($x10)*10;
# errorbars
my $x37 = xvals(37);
my $x37_2 = $x37*2;
my $x37sqrd = (xvals(37)/3)**2;
my $x90 = xvals(90);
my $sin90 = sin($x90*4*PI()/90)*30 + 72;
my $x90_2 = $x90/2;
my $ones_90 = ones(90)*110;
# Image & circles plot
my $x11 = xvals(11,11);
my $y11 = yvals(11,11);
my $r11 = rvals(11,11);
my $x15 = xvals(15);
my $x15_15 = $x15*1.5;
my $sin15 = sin(xvals(15))**2 * 4;
my $x500 = xvals(500)+1;
my $x5 = xvals(5);
# multi
my $r9 = rvals(9,9);
my $r9minus = -$r9;
my $s9 = sequence(9,9);
my $xyr9 = pdl(xvals(9,9),yvals(9,9),$r9)*20;
# Test imag
my $x100 = xvals(100,100);
my $y100 = yvals(100,100);
my $im = 1000 * sin(rvals(100,100)/3) / (rvals(100,100)+30);

my $pgplot_ran = 0;
for my $engine (@engines) {
    my $w;

    my $module;
    my $mod_hash = $mods->{$engine};
    diag("skipping $engine as unregistered"), next if !$mod_hash; # if didn't register
    ok( ( ref($mod_hash) eq 'HASH' and ($module = $mod_hash->{module}) ),
	"there is a modules entry for $engine ($module)" );

    SKIP: {
      my $check_ok = eval {$module->can('check')->(1)};
      is($@, '', "${module}::check() ran OK");
      diag "module '$engine' registration hash: ", explain $mod_hash;

      unless($check_ok) {
	  diag qq{Skipping $module: $mod_hash->{msg}};
	  skip "Skipping tests for engine $engine (not working)", $tests_per_engine - 2;
      }
      $pgplot_ran ||= $engine eq 'pgplot';

      eval { $w = PDL::Graphics::Simple->new(engine=>$engine, multi=>[3,2]) };
      is($@, '', "constructor for $engine worked OK");
      isa_ok($w, 'PDL::Graphics::Simple', "constructor for $engine worked OK");

##############################
# Simple line & bin plot
{
my @args = PDL::Graphics::Simple::_translate_plot(@$w{qw(held keys)},
  with=>'line', $x10, $x10sqrt,
  with=>'bins', $x10, $sin10,
  {title=>"PDL: $engine engine, line & bin plots"}
);
is_deeply \@args, [
  [ 'line 1', 'bin 2' ],
  {
    'bounds' => undef, 'crange' => undef,
    'justify' => 0, 'legend' => undef,
    'logaxis' => '', 'oplot' => 0,
    'title' => "PDL: $engine engine, line & bin plots", 'wedge' => '',
    'xlabel' => undef, 'ylabel' => undef,
    'xrange' => [ 0, 9 ],
    'yrange' => [ '-9.58924274663138', '9.89358246623382' ]
  },
  [
    { 'key' => undef, 'style' => undef, 'width' => undef, 'with' => 'lines' },
    $x10, $x10sqrt,
  ],
  [
    { 'key' => undef, 'style' => undef, 'width' => undef, 'with' => 'bins' },
    $x10, $sin10,
  ]
];
}
      eval { $w->plot(with=>'line', $x10, $x10sqrt,
		      with=>'bins', $sin10,
		 {title=>"PDL: $engine engine, line & bin plots"}),
      };
      is($@, '', "plot succeeded\n");

##############################
# Error bars plot
{
my @args = PDL::Graphics::Simple::_translate_plot(@$w{qw(held keys)},
  with=>'errorbars', $x37_2, $x37sqrd, $x37,
  with=>'limitbars', $x90, $sin90, $x90_2, $ones_90,
  {title=>"PDL: $engine engine, error (rel.) & limit (abs.) bars"},
);
is_deeply \@args, [
  [ 'errorbar 1', 'limitbar 2' ],
  {
    'bounds' => undef, 'crange' => undef,
    'justify' => 0, 'legend' => undef,
    'logaxis' => '', 'oplot' => 0,
    'title' => "PDL: $engine engine, error (rel.) & limit (abs.) bars",
    'wedge' => '',
    'xlabel' => undef, 'ylabel' => undef,
    'xrange' => [ 0, 89 ], 'yrange' => [ 0, 144 ]
  },
  [
    { 'key' => undef, 'style' => undef, 'width' => undef, 'with' => 'errorbars' },
    $x37_2, $x37sqrd, $x37,
  ],
  [
    { 'key' => undef, 'style' => undef, 'width' => undef, 'with' => 'limitbars' },
    $x90, $sin90, $x90_2, $ones_90,
  ]
];
}
      eval { $w->plot( with=>'errorbars', $x37_2, $x37sqrd, $x37,
		       with=>'limitbars', $sin90, $x90_2, $ones_90,
		       {title=>"PDL: $engine engine, error (rel.) & limit (abs.) bars"}
		 ); };
      is($@, '', "errorbar plot succeeded");

##############################
# Image & circles plot
{
my @args = PDL::Graphics::Simple::_translate_plot(@$w{qw(held keys)},
  with=>'image', $x11, $y11, $r11,
  with=>'circle', $x15, $x15_15, $sin15,
  {title=>"PDL: $engine engine, image & circle plots (not justified)"},
);
is_deeply \@args, [
  [ 'image 1', 'circle 2' ],
  {
    'bounds' => undef, 'crange' => undef,
    'justify' => 0, 'legend' => undef,
    'logaxis' => '', 'oplot' => 0,
    'title' => "PDL: $engine engine, image & circle plots (not justified)",
    'wedge' => '',
    'xlabel' => undef, 'ylabel' => undef,
    'xrange' => [ '-0.5', 14 ], 'yrange' => [ '-0.5', 21 ]
  },
  [
    { 'key' => undef, 'style' => undef, 'width' => undef, 'with' => 'image' },
    $x11, $y11, $r11,
  ],
  [
    { 'key' => undef, 'style' => undef, 'width' => undef, 'with' => 'circles' },
    $x15, $x15_15, $sin15,
  ]
];
}
      eval { $w->plot(with=>'image', $r11,
		      with=>'circle', $x15, $x15_15, $sin15,
		      {title=>"PDL: $engine engine, image & circle plots (not justified)"}
		 );
      };
      is($@, '', "plot succeeded\n");

##############################
# Image & circles plot (justified)
{
my @args = PDL::Graphics::Simple::_translate_plot(@$w{qw(held keys)},
  with=>'image', $x11, $y11, $r11,
  with=>'circle', $x15, $x15_15, $sin15,
  {title=>"PDL: $engine engine, image & circle plots (not justified)", j=>1},
);
is_deeply \@args, [
  [ 'image 1', 'circle 2' ],
  {
    'bounds' => undef, 'crange' => undef,
    'justify' => 1, 'legend' => undef,
    'logaxis' => '', 'oplot' => 0,
    'title' => "PDL: $engine engine, image & circle plots (not justified)",
    'wedge' => '',
    'xlabel' => undef, 'ylabel' => undef,
    'xrange' => [ '-0.5', 14 ], 'yrange' => [ '-0.5', 21 ]
  },
  [
    { 'key' => undef, 'style' => undef, 'width' => undef, 'with' => 'image' },
    $x11, $y11, $r11,
  ],
  [
    { 'key' => undef, 'style' => undef, 'width' => undef, 'with' => 'circles' },
    $x15, $x15_15, $sin15,
  ]
] or diag explain \@args;
}
      eval { $w->plot(with=>'image', $r11,
		      with=>'circle', $x15, $x15_15, $sin15,
		      {title=>"PDL: $engine engine, image & circle plots (justified)", j=>1}
		 );
      };
      is($@, '', "justified image and circles plot succeeded");

##############################
# Text
{
my @args = PDL::Graphics::Simple::_translate_plot(@$w{qw(held keys)},
  with=>'labels',
  $x5, $x5,
  ["<left-justified","<    left-with-spaces", "|centered","|>start with '>'",">right-justified"],
  {title=>"PDL: $engine engine, text on graph", yrange=>[-1,5] }
);
is_deeply \@args, [
  [],
  {
    'bounds' => undef, 'crange' => undef,
    'justify' => 0, 'legend' => undef,
    'logaxis' => '', 'oplot' => 0,
    'title' => "PDL: $engine engine, text on graph", 'wedge' => '',
    'xlabel' => undef, 'ylabel' => undef,
    'xrange' => [ 0, 4 ], 'yrange' => [ -1, 5 ]
  },
  [
    { 'key' => undef, 'style' => undef, 'width' => undef, 'with' => 'labels' },
    $x5, $x5,
    [
      '<left-justified', '<    left-with-spaces',
      '|centered', '|>start with \'>\'', '>right-justified'
    ]
  ]
];
}
      eval { $w->plot(with=>'labels',
		      $x5, $x5,
		      ["<left-justified","<    left-with-spaces", "|centered","|>start with '>'",">right-justified"],
		      {title=>"PDL: $engine engine, text on graph", yrange=>[-1,5] }
		 );
      };
      is($@, '', "labels plot succeeded" );

##############################
# Log scaling
{
my @args = PDL::Graphics::Simple::_translate_plot(@$w{qw(held keys)},
  with=>'line',$x500,$x500,{log=>'y',title=>"PDL: $engine engine, Y=X (semilog)"}
);
is_deeply \@args, [
  [ 'line 1' ],
  {
    'bounds' => undef, 'crange' => undef,
    'justify' => 0, 'legend' => undef,
    'logaxis' => 'y', 'oplot' => 0,
    'title' => "PDL: $engine engine, Y=X (semilog)", 'wedge' => '',
    'xlabel' => undef, 'ylabel' => undef,
    'xrange' => [ 1, 500 ], 'yrange' => [ 1, 500 ]
  },
  [
    { 'key' => undef, 'style' => undef, 'width' => undef, 'with' => 'lines' },
    $x500, $x500,
  ]
];
}
      eval { $w->plot(with=>'line',$x500,{log=>'y',title=>"PDL: $engine engine, Y=X (semilog)"}); };
      is($@, '', "log scaling succeeded");

      ask_yn qq{
Testing $engine engine: You should see in a 3x2 grid:
1) a superposed line plot and bin plot, with x range from 0 to 9 and
yrange from 0 to 9. The two plots should have different line styles.
2) error bars (symmetric relative to each plotted point) and limit bars
(asymmetric about each plotted point).
3) a radial 11x11 "target" image and some superimposed "circles".
Since the plot is not justified, the pixels in the target image should
be oblong and the "circles" should be ellipses.
4) the same plot as (3), but justified.  superimposed "circles".
Since the plot is justified, the pixels in the target image should be
square and the "circles" should really be circles.
5) "left-justified" text left aligned on x=0, "left-with-spaces" just
right of x=1, "centered" centered on x=2, ">start with '>'" centered on
x=3, and "right-justified" right-aligned on x=4.
6) a simple logarithmically scaled plot, with appropriate title.}, "plots look OK";

##############################
# Multiplot
{
my @new = PDL::Graphics::Simple::_translate_new(multi=>[2,2]);
# ignore $engine
is_deeply $new[1], {
  'multi' => [2,2], 'output' => '', 'size' => [ 8, 6, 'in' ], 'type' => 'i'
} or diag explain \@new;
}
      eval { $w=PDL::Graphics::Simple->new(engine=>$engine, multi=>[2,2]); };
      is($@, '', "Multiplot declaration was OK");
      $w->image( $r9,{wedge=>1} ); $w->image( $r9minus,{wedge=>1} );
      $w->image( $s9 );            $w->image( $xyr9 );
      ask_yn qq{Testing $engine engine: You should see two bullseyes across the top (one in
negative print), a gradient at bottom left, and an RGB blur (if supported
by the engine - otherwise a modified gradient) at bottom right.  The top two
panels should have colorbar wedges to the right of the image.}, "multiplot OK";
    }
}


# Continue the simple engine and convenience interfaces
{
my $type = 'image';
my $me = PDL::Graphics::Simple::_invocant_or_global();
my @imag_args = PDL::Graphics::Simple::_translate_imag($me, $im);
shift @imag_args; # $me
my @args = PDL::Graphics::Simple::_translate_plot(@$me{qw(held keys)}, PDL::Graphics::Simple::_translate_convenience($type, $x100, $y100, @imag_args));
is_deeply \@args, [
  [ 'image 1' ],
  {
    'bounds' => undef, 'crange' => [ undef, undef ],
    'justify' => 1, 'legend' => undef,
    'logaxis' => '', 'oplot' => 0,
    'title' => undef, 'wedge' => '',
    'xlabel' => undef, 'ylabel' => undef,
    'xrange' => [ '-0.5', '99.5' ], 'yrange' => [ '-0.5', '99.5' ]
  },
  [
    { 'key' => undef, 'style' => undef, 'width' => undef, 'with' => 'image' },
    $x100, $y100, $im
  ]
];
}
eval { imag $im };
is($@, '', "imag worked with no additional arguments" );
ask_yn q{  test> $im = 1000 * sin(rvals(100,100)/3) / (rvals(100,100)+30);
  test> imag $im;
You should see a bullseye pattern with a brighter inner ring.}, "bullseye OK";

{
my $type = 'image';
my $me = PDL::Graphics::Simple::_invocant_or_global();
my @imag_args = PDL::Graphics::Simple::_translate_imag($me, $im, {wedge=>1, title=>"Bullseye!"});
shift @imag_args; # $me
my @args = PDL::Graphics::Simple::_translate_plot(@$me{qw(held keys)}, PDL::Graphics::Simple::_translate_convenience($type, $x100, $y100, @imag_args));
is_deeply \@args, [
  [ 'image 1' ],
  {
    'bounds' => undef, 'crange' => [],
    'justify' => 1, 'legend' => undef,
    'logaxis' => '', 'oplot' => 0,
    'title' => 'Bullseye!', 'wedge' => 1,
    'xlabel' => undef, 'ylabel' => undef,
    'xrange' => [ '-0.5', '99.5' ], 'yrange' => [ '-0.5', '99.5' ]
  },
  [
    { 'key' => undef, 'style' => undef, 'width' => undef, 'with' => 'image' },
    $x100, $y100, $im
  ]
] or diag explain \@args;
}
eval { imag $im, {wedge=>1, title=>"Bullseye!"} };
is($@, '', "imag worked with plot options");
ask_yn q{  test> imag $im, {wedge=>1, title=>"Bullseye!", j=>1};
You should see the same image, but with a colorbar wedge on the right; a title
up top; and a justified aspect ratio (circular rings). The color scale may be
slightly less contrasty than the last frame, because some engines extend the
colorbar wedge to round numbers.}, "justified bullseye and wedge OK";

{
my $type = 'image';
my $me = PDL::Graphics::Simple::_invocant_or_global();
my @imag_args = PDL::Graphics::Simple::_translate_imag($me, $im, 0, 30, {wedge=>1, j=>1});
shift @imag_args; # $me
my @args = PDL::Graphics::Simple::_translate_plot(@$me{qw(held keys)}, PDL::Graphics::Simple::_translate_convenience($type, $x100, $y100, @imag_args));
is_deeply \@args, [
  [ 'image 1' ],
  {
    'bounds' => undef, 'crange' => [0,30],
    'justify' => 1, 'legend' => undef,
    'logaxis' => '', 'oplot' => 0,
    'title' => undef, 'wedge' => 1,
    'xlabel' => undef, 'ylabel' => undef,
    'xrange' => [ '-0.5', '99.5' ], 'yrange' => [ '-0.5', '99.5' ]
  },
  [
    { 'key' => undef, 'style' => undef, 'width' => undef, 'with' => 'image' },
    $x100, $y100, $im
  ]
] or diag explain \@args;
}
eval { imag $im, 0, 30, {wedge=>1, j=>1} };
is($@, '', "imag worked with bounds");
ask_yn q{  test> imag $im, 0, 30, {wedge=>1, j=>1};
You should see the same image, but with no title and with a tighter
dynamic range that cuts off the low values (black rings instead of
the fainter parts of the bullseye).}, "crange shortcut is OK";

eval { erase };
is($@, '', "erase executed");
my $extra = $pgplot_ran ? ' (for PGPLOT on X you need to close the X window to continue)' : '';
ask_yn qq{  test> erase
The window should have disappeared$extra.}, "erase worked";

done_testing;
