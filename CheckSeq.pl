#!/usr/common/bin/perl.new -w

# CRC=4110317807

use strict;
use FileHandle;

my $SeqFile = $ENV{SEQFILE} || '/home/njas/gauss/hisdir/strippedfry';
my $MinTerms = $ENV{MINTERMS} || 0;
$MinTerms = 20 unless ($MinTerms >= 1);

# %S A088674 ,1,3,6,45,126,750,2796,19389,75894,449562,2027796,12211794,57895596,332787324,1677545304,9766642077,50378641830,286825948194,1529968671492,8729259097158,47374697101572,269062276076868,1484430536591592,
# %S A088675 ,0,1,2,8,36,160,656,2368,7664,29440,184896,1174272,3395200,21222400,178961920,1638189056,27449296640,28875071488,3234263731200,10138343231488,422012179953664,3426627065331712,59293997091528704,
# %S A088676 ,1,2,2,2,3,3,3,3,4,4,5,5,5,5,6,6,6,6,6,6,7,7,8,9,9,9,9,9,9,9,10,11,12,12,14,
# %S A088677 ,2,1,2,3,1,2,5,6,2,5,1,4,5,11,6,9,10,11,13,14,8,17,11,4,11,16,19,20,22,27,16,22,4,23,24,
# %S A088679 ,0,1,2,6,48,2880,9953280,115579079884800,15266884236590834264309760000,262212473580148912869121218589990322256745385164800000000,
# %S A088682 ,4,6,10,10,10,8,8,14,6,18,8,8,10,12,6,16,10,16,8,6,18,18,12,14,10,12,12,8,14,6,12,10,20,16,8,12,12,14,6,8,10,18,14,12,12,24,12,6,18,18,6,12,12,20,12,18,8,8,12,24,6,14,28,18,12,16,8,22,6,8,6,8,12,28,6,14,8,12,6,24,
# %S A088683 ,6,6,8,6,12,10,10,12,6,18,12,12,12,12,14,6,8,12,8,12,6,20,6,14,10,12,12,10,12,16,12,18,24,12,16,8,10,22,10,14,14,18,12,14,12,22,12,12,6,18,24,18,10,18,14,16,12,16,12,22,10,14,24,18,14,10,8,28,10,10,16,40,14,24,6,
# %S A088684 ,6,6,8,6,8,6,10,6,6,10,12,10,8,6,24,6,12,12,10,24,6,16,10,12,14,18,12,10,6,20,12,14,16,8,16,8,6,12,12,16,18,18,10,10,18,14,6,24,6,6,24,18,12,10,14,10,12,12,8,6,12,12,12,16,20,12,18,20,8,6,14,40,26,18,10,6,22,6,
# %S A088680 ,2,4,4,4,2,4,4,6,6,2,4,8,2,2,14,6,10,6,4,6,10,4,12,4,4,2,6,6,6,2,14,2,14,10,4,8,6,6,4,10,10,6,6,4,4,8,8,6,2,6,6,2,10,6,6,4,12,2,6,2,4,8,8,8,6,8,4,4,10,2,2,2,14,2,14,2,20,8,8,6,14,6,8,12,6,10,6,2,2,18,2,6,8,6,2,
# %S A088681 ,2,2,2,6,6,2,6,2,4,6,6,4,4,4,4,2,2,6,6,2,2,2,12,2,6,10,6,2,4,10,4,4,6,2,6,6,4,8,8,2,2,4,8,2,12,4,4,12,18,10,6,6,6,2,6,2,10,4,6,12,6,10,10,6,4,6,8,14,12,10,4,10,4,4,4,4,4,10,4,6,4,6,6,4,2,2,10,10,6,4,4,6,6,22,10,


# Each element may occur at more than one position.
# Index a hash by element, and store a hash of the positions
# at which that element occurs.

sub hash_elements {
    my $elem_ref = shift;
    my ($i, %hash);

    for ($i = 0; $i < @$elem_ref; ++$i) {
	$hash{$elem_ref->[$i]}{$i} = 1;
    }
    return \%hash;
}


# To constitute a valid overlap,
# a) one sequence must start (or end) within the other, and
# b) the rest (or previous part) of that sequence must match
#    until either sequence is exhausted
# The qhash allows quick checks for presence of an element
# in the query sequence, and, if present, where it occurs.

sub check_overlap {
    my ($seqno, $need, $qhash, $qelem, $elems) = @_;
    my ($elem, $posns, $posn, $span, $i);

    $need = @$elems if ($need > @$elems);
    $elem = $elems->[0];	# Maybe other starts somewhere in query
    if (exists($qhash->{$elem})) {
	$posns = $qhash->{$elem};
POS1:
	foreach $posn (sort {$a <=> $b} keys(%$posns)) {
	    $span = (@$qelem - $posn);	# How much of query sequence follows
	    $span = @$elems if (@$elems < $span);	# other ends first
	    last unless ($span >= $need);	# Too short and getting shorter
	    for ($i = 1; $i < $span; ++$i) {	# index 0 known to match
		next POS1 unless ($qelem->[$posn + $i] eq $elems->[$i]);
	    }
	    print("$seqno: $posn 0 $span\n");
	    return;
	}
    }
    $elem = $elems->[-1];	# Maybe other ends somewhere in query
    if (exists($qhash->{$elem})) {
	$posns = $qhash->{$elem};
POS2:
	foreach $posn (sort {$b <=> $a} keys(%$posns)) {
	    $span = $posn + 1;	# Length of query sequence ending here
	    $span = @$elems if (@$elems < $span);	# other starts later
	    last unless ($span >= $need);	# Too short and getting shorter
	    for ($i = $span; --$i > 0; ) {
		next POS2
		    unless ($qelem->[$posn - $i] eq $elems->[-$i - 1]);
	    }
	    $posn -= $span - 1;		# first query position in match
	    $i = @$elems - $span;	# first position in other
	    print("$seqno: $posn $i $span\n");
	    return;
	}
    }
    return;
}


sub main {
    my $fh = new FileHandle;
    my ($qline, $line, @query_elem, $query);
    my ($seqno, @elems, $seq, $need);
    my ($i, $posn, $span);

    open($fh, "< $SeqFile") || die("Open failed on sequence file $SeqFile: $!");
    while ($qline = <>) {
	chomp($qline);
	@query_elem = split(/[^-0-9]+/ , $qline);
	shift(@query_elem) while (@query_elem && ($query_elem[0]  !~ /\d+/));
	pop(@query_elem)   while (@query_elem && ($query_elem[-1] !~ /\d+/));
	if (@query_elem) {
	    $qline = join(',' => '', @query_elem, '');
	    $query = hash_elements(\@query_elem);
	    $need = (@query_elem <= 1.2 * $MinTerms) ?
			int(.75 * @query_elem) : $MinTerms;
	    $need = 1 if ($need < 1);
	    seek($fh, 0, 0) || die("Rewind failed on $SeqFile: $!");
	    while ($line = <$fh>) {
		(undef, $seqno, $seq) = split(" ", $line);
		if (($i = index($seq, $qline)) >= 0) {
		    $posn = ($i == 0) ? 0 : (substr($seq, 0, $i-1) =~ tr/,//);
		    $span = @query_elem;
		    print("$seqno: 0 $posn $span\n");
		    next;
		}
		# Trailing empty field won't be included
		@elems = split(',' => $seq);
		shift(@elems);	# Eliminate empty leading field
		check_overlap($seqno, $need, $query, \@query_elem, \@elems)
		    if (@elems);
	    }
	}
    }
}
main();

__END__

=head1 NAME

CheckSeq.pl - Check query sequences against sequence database

=head1 SYNOPSIS

CheckSeq.pl F<query-file> ...

=head1 EXAMPLE

  # Run queries against standard database
  CheckSeq.pl queries
  # Same, but reset minimum overlap threshold
  MINTERMS=10 CheckSeq.pl queries
  # Use a different sequence database
  SEQFILE=/some/other/database CheckSeq.pl queries

=head1 DESCRIPTION

B<CheckSeq.pl> reads query sequences,
one per line, from the specified F<query-file>s,
or from F<standard in> if no F<query-file>s are specified.
For each sequence in the sequence database
that "overlaps adequately" with a query sequence,
a one-line summary resembling the following is produced.

  A088676: 3 0 34

The first value is the name of the matching sequence from
the sequence database.  The three numeric values that follow are

=over 4

=item 1

The position (starting at B<0>) in the query match
of the overlapping subsequence.

=item 2

The position (starting at B<0>) in the database sequence
of the overlapping subsequence.

=item 3

The length of the overlapping subsequence.

=back 4

Roughly speaking, two sequences I<overlap>
if you can "line up" the elements of the sequences
so they agree on some terms, and disagree on none.

More formally, we will say a sequence B<S1> I<overlaps> a sequence B<S2>
if there exist indexes B<I1> and B<I2>
and a length B<L> E<gt> B<0> such that
for all B<I> E<lt> B<L>,

   S1[I1 + I] == S2[I2 + I]

and at least one of the following conditions hold.

=over 4

=item

The overlap includes the entire B<S1> sequence.

=item

The overlap includes the entire B<S2> sequence.

=item

The overlap includes the start of one sequence and the end of the other.

=back 4

Since overlap, thus defined, always includes the start of at least
one sequence, at least one of the positions reported for a
match will always be B<0>.

Small overlaps of long sequences are not particularly noteworthy.
The value of environment variable B<MINTERMS> (default B<20>)
establishes a lower limit of the length of an I<adequate> overlap
for long sequences.
Of course, short query sequences cannot be held to a fixed standard.
So, for query sequences shorter than B<1.2 * MINTERMS>,
an overlap of B<.75 * length(query)> is considered I<adequate>.
And, if even this exceeds the length of a database sequence,
the length of the database sequence is considered I<adequate>.

=head1 FILES AND FORMATS

The default B<SEQFILE> database is
F</home/njas/gauss/hisdir/strippedfry>.
It contains lines of the form

  %S A077013 ,1,2,1,2,3,7,14,43,65,292,992,1154,

The C<%S> is just fixed text.
The word that follows, C<A077013>,
is the sequence name.
The string that follows is the sequence.
Note the leading and trailing commas,
and the absence of white space or signs.
The entire sequence must appear on a single line.
Any substitute B<SEQFILE> must have the same format.

Query sequences are less restricted.
They must still appear on a single line,
but the terms in the sequence can be separated by blanks,
commas, or, indeed, any non-empty sequence
of non-numeric characters.

=head1 AUTHOR
 
     John P. Linderman
     AT&T Shannon Laboratory
     jpl@research.att.com

=head1 BUGS

The determination of what constitutes an I<adequate> overlap
is a bit ad-hoc.

If you submit more than one query,
the summary information doesn't provide information
about which query matched.

=cut
