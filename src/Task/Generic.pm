##
# MODULE DESCRIPTION: Task with injected iterate method.
#
# This task allows one to create a Task without having to create a Perl package.
# Also, a non-Task package can be used as a Task package.
# Includes subtask support.
#
# <h3>Example</h3>
# <pre class="example">
# # Task with no package.
# new Task::Generic(
#     iterate => sub { my ( $task ) = @_; message "Example\n"; $task->stop; },
# );
#
# # Task with a package which does not inherit from Task.
# new Task::Generic(
#     package => 'MyPackage',
#     iterate => 'my_method',
# );
# package MyPackage;
# sub my_method { my ( $task ) = @_; message "Example\n"; $task->stop; }
# </pre>
package Task::Generic;

use strict;
use base 'Task::WithSubtask';
use Modules 'register';

### CATEGORY: Constructor

##
# Task::Generic->new(...)
#
# The following options are allowed:
# `l
# - package - Perl package which implements task_iterate.
# - iterate - name of Perl function in package which should be called instead of task_iterate, if any.
# All other options are stored for the pacakge to use.
# `l`
sub new {
	my ($class, %args) = @_;

    $args{package} ||= (caller)[0];
    $args{iterate} ||= 'task_iterate';

	my $self = $class->SUPER::new(%args);
	$self->{$_} ||= $args{$_} foreach keys %args;

	$self;
}

sub iterate {
	my ( $self ) = @_;

	# Run subtasks, if any.
	return if !$self->SUPER::iterate;

	my $sub = $self->{iterate};
	if ( !ref $sub || ref $sub ne 'CODE' ) {
		$sub = \&{"$self->{package}::$self->{iterate}"};
	}
	$sub->( $self );
}

1;
