use strict;
use warnings;

my @results;

my $old_depth = 0;
my $cur_depth = 0;
my @path;

foreach (<STDIN>) {
    foreach my $name (@ARGV) {
        if ($_ =~ /^(\*+)\s(.*)$/) {
            $cur_depth = length $1;
            if ($cur_depth > $old_depth) {
                push @path, $2;
            } elsif ($cur_depth == $old_depth) {
                pop @path;
                push @path, $2;
            } else {
                pop @path;
            }
            $old_depth = $cur_depth;
        } elsif ($_ =~ /^-\s($name)\s::\s(.*)$/) {
            my $password = $2;
            my $p = join '/', @path;
            # @path = ();
            print "$p/$name: $2\n"
        }
    }
}
