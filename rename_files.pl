#!/usr/bin/perl 
#!/usr/bin/perl -w
main();

#### to rename multiple files with mv (on linux) ####

sub main {
  @files_target = </disks/PROJECT/abc/Data/Cache/*QC.RData>;
  foreach $fileqc (@files_target) {
    ##print $fileqc . "\n";
    my $filenewqc = $fileqc;
    $filenewqc =~ s/QC/NEW_CC_QC/;      
    ##print $filenewqc . "\n";
    system "mv $fileqc $filenewqc";
  }
}