
use strict;
use warnings;

use ExtUtils::MakeMaker;

WriteMakefile(
  DISTNAME  => 'CGI-Application-Plugin-RunmodeDeclare',
  NAME      => 'CGI::Application::Plugin::RunmodeDeclare',
  AUTHOR    => 'Rhesa\ Rozendaal\ \<rhesa\@cpan\.org\>',
  ABSTRACT  => 'Declare cgiapp runmodes with keywords',
  VERSION   => '0.07',
  EXE_FILES => [ qw() ],
  (eval { ExtUtils::MakeMaker->VERSION(6.21) } ? (LICENSE => 'perl') : ()),
  PREREQ_PM    => {
    "CGI::Application" => '4.06',
    "Devel::Declare" => '0.003001',
  },
);
