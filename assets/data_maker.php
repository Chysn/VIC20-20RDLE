<?php

// Usage
// php data_maker.php > ../asm/word_list.asm

$index = "            .word $0000\n"; // Table of addresses of first letters
$data = ''; // Table of last four letters
$word_list = file('word_list.txt');
$common_words = file('common_words.txt');

$last_first_letter = '';
$addr = 0; // Offset address
$common = 0;
foreach ($word_list as $word)
{
    $first_letter = substr($word, 0, 1);
    if ($first_letter != $last_first_letter) {
        $h = str_pad(dechex($addr), 4, '0', STR_PAD_LEFT);
        $index .= "            .word \${$h} ; Offset {$first_letter}\n";
        $last_first_letter = $first_letter;
    }
    
    // Pack the last four letters of each word into three bytes
    // Each letter takes five bits, arranged like this, where
    // b is letter 2, c is letter 3, d is letter 4, e is letter 5
    // Byte 0: 1bbbbbcc
    // Byte 1: 0cccdddd
    // Byte 2: 0deeeee0
    $last4 = str_split(substr($word, 1, 4));
    
    // Convert each word into a list of 5-bit values from 1 to 26 for A-Z
    for ($i = 0; $i < 4; $i++)
    {
        $last4[$i] = ord($last4[$i]) - 96;
    }
    $byte1 = 128 + ($last4[0] * 4) + (($last4[1] & 24) / 8); // All of b, high 2 of c
    $byte2 = (($last4[1] & 7) * 16) + (($last4[2] & 30) /2); // low 3 of c, high 4 of d
    $byte3 = (($last4[2] & 1) * 64) + ($last4[3] * 2); // low 1 of d, all of e

    $is_common = in_array($word, $common_words) ? 1 : 0;
    $common += $is_common;
    $byte3 += $is_common; // Set bit 0 of byte 3 for a game word

    // And make these data bytes
    $v1 = str_pad(dechex($byte1), 2, '0', STR_PAD_LEFT);
    $v2 = str_pad(dechex($byte2), 2, '0', STR_PAD_LEFT);
    $v3 = str_pad(dechex($byte3), 2, '0', STR_PAD_LEFT);

    $line .= "\${$v1},\${$v2},\${$v3},";
    if (++$ln == 5) {
        $ln = 0;
        $data .= "            .byte " . trim($line, ',') . " ;{$first_letter}\n";
        $line = '';
    }
        
    $addr += 3;
}
if ($line) {
    $data .= "            .byte " . trim($line, ',') . " ;{$first_letter}\n";
}
$data .= "            .byte \$ff,\$ff,\$ff ; End of word list\n";

$size = sizeof($word_list);
$hex_size = dechex($size);
$h = str_pad(dechex($addr), 4, '0', STR_PAD_LEFT);
$index .= "            .word \${$h} ; End-Of-List offset\n";

print "ListSize:   .word \${$hex_size} ; {$size} words ({$common} common)\n";
print "AlphInd:" . substr($index, 8);
print "WordList:" . substr($data, 9);
