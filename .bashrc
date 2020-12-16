# /etc/skel/.bashrc
#
# This file is sourced by all *interactive* bash shells on startup,
# including some apparently interactive shells such as scp and rcp
# that can't tolerate any output.  So make sure this doesn't display
# anything or bad things will happen !


# Test for an interactive shell.  There is no need to set anything
# past this point for scp and rcp, and it's important to refrain from
# outputting anything in those cases.
if [[ $- != *i* ]] ; then
	# Shell is non-interactive.  Be done now!
	return
fi


# Put your fun stuff here.
export LANG=en_US.UTF-8
export LC_ALL=en_US.UTF-8

[ -f "${HOME}"/.script/setup ] && source "${HOME}"/.script/setup
[ -f "${HOME}"/.bash_untracked_profile ] && source "${HOME}"/.bash_untracked_profile

true # exit 0
