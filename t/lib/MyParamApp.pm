package MyParamApp;
our $VERSION = '0.08';


use base qw/MyApp1/;
use CGI::Application::Plugin::RunmodeDeclare;

startmode test ($id) { "id=$id" }
runmode array (@stuff) { "stuff=@stuff;" }
1;
