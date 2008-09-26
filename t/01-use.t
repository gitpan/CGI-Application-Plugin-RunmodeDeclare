use strict;
use warnings;

use Test::More 'no_plan';

use lib 't/';
use_ok 'MyApp1';

my $app1 = MyApp1->new;
$app1->mode_param(sub { 'other' } ); # "start" actually needs cgiapp > 4.11
my %modes = $app1->run_modes;

for my $m (qw( begin other )) {
    ok MyApp1->can($m), "$m is a method";
    is $modes{$m}, $m, "$m is a run mode";
}
ok MyApp1->can('oops'), 'oops is a method...';
ok ! exists $modes{'oops'}, "but it's not a run mode, it's the error mode";

my $out = $app1->run;
is $out, 'other', 'and runmode "other" outputs "other"';

__END__
t/01-use....
ok 1 - use MyApp1;
ok 2 - begin is a method
ok 3 - begin is a run mode
ok 4 - other is a method
ok 5 - other is a run mode
ok 6 - oops is a method...
ok 7 - but it's not a run mode, it's the error mode
ok 8 - and runmode "other" outputs "other"
1..8
ok
All tests successful.
Files=1, Tests=8,  0 wallclock secs ( 0.04 usr  0.00 sys +  0.08 cusr  0.00 csys =  0.12 CPU)
Result: PASS
