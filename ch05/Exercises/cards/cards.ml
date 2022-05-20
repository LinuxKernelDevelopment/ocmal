type suit = Spade | Heart | Diamond | Club

type rank = Two | Three | Four | Five | Six | Seven | Eight | Nine | Ten | Jack | Queen | King | Ace

type card = { s : suit; r : rank}

let ac = { s = Club; r = Ace }

let qh = { s = Heart; r = Queen}

let td = { s = Diamond; r = Two}

let ss = { s = Spade; r = Seven}
