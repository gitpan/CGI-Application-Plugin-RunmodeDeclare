package MyApp1;

BEGIN {$ENV{'CGI_APP_RETURN_ONLY'} = 1}

use base 'CGI::Application';
use CGI::Application::Plugin::RunmodeDeclare;

sub cgiapp_prerun { shift->header_type('none') }
startmode begin { "Go" }
errormode oops { "Oh noez!" }
runmode start ($c:) { return $c->get_current_runmode }

1;