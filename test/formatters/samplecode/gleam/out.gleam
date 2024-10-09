fn encode_(dna: List(Nucleotide), acc: BitArray) {
  case dna {
    [] -> acc
    [first, ..rest] -> {
      let nbit = encode_nucleotide(first)
      encode_(rest, <<acc:bits, nbit:size(2)>>)
    }
  }
}

pub fn encode(dna: List(Nucleotide)) -> BitArray {
  encode_(dna, <<>>)
}
