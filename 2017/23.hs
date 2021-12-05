import Control.DeepSeq

data Op = Set Char Int
        | SetReg Char Char
        | Add Char Int
        | AddReg Char Char
        | SubReg Char Char
        | Mul Char Int
        | MulReg Char Char
        | JumpNZ Int Int
        | JumpNZReg Char Int deriving (Eq,Show)

-- ops, pc, regs, muls
type Machine = ([Op], Int, [(Char, Int)], Int)

parse :: [String] -> Op
parse (op:loc:more)
    | op == "set" && immediate = Set reg num
    | op == "set" = SetReg reg rreg
    | op == "add" && immediate = Add reg num
    | op == "add" = AddReg reg rreg
    | op == "sub" && immediate = Add reg (negate num)
    | op == "sub" = SubReg reg rreg
    | op == "mul" && immediate = Mul reg num
    | op == "mul" = MulReg reg rreg
    | op == "jnz" && elem (head loc) digits = JumpNZ (read loc) num
    | op == "jnz" = JumpNZReg reg num
    where reg = head loc
          rreg = head $ head more
          digits = "-0123456789"
          immediate = elem rreg digits
          num = read $ head more

readReg :: [(Char,Int)] -> Char -> Int
readReg mem reg
    | length regs == 0 = 0
    | otherwise = snd $ head regs
    where regs = filter (\(r,_) -> r == reg) mem

writeReg :: [(Char,Int)] -> Char -> Int -> [(Char,Int)]
writeReg mem reg val = deepseq newmem newmem
    where newmem = (reg,val) : filter (\(r,_) -> r /= reg) mem

execute :: Machine -> Op -> Machine
execute (ops,pc,mem,muls) (Set reg val) = (ops, pc+1, writeReg mem reg val, muls)
execute m@(_,_,mem,_) (SetReg reg vreg) = execute m (Set reg (readReg mem vreg))
execute (ops,pc,mem,muls) (Add reg val) = (ops, pc+1, writeReg mem reg (val + readReg mem reg), muls)
execute m@(_,_,mem,_) (AddReg reg vreg) = execute m (Add reg (readReg mem vreg))
execute m@(_,_,mem,_) (SubReg reg vreg) = execute m (Add reg (negate $ readReg mem vreg))
execute (ops,pc,mem,muls) (Mul reg val) = (ops, pc+1, writeReg mem reg (val * readReg mem reg), succ muls)
execute m@(_,_,mem,_) (MulReg reg vreg) = execute m (Mul reg (readReg mem vreg))
execute (ops,pc,mem,muls) (JumpNZ cond val)
    | cond /= 0 = (ops, pc+val, mem, muls)
    | otherwise = (ops, pc+1, mem, muls)
execute m@(_,_,mem,_) (JumpNZReg reg num) = execute m (JumpNZ (readReg mem reg) num)


run :: Machine -> Machine
run m@(ops,pc,_,_)
    | pc >= length ops = m
    | otherwise = run $ execute m (ops !! pc)

factors :: Int -> [Int]
factors n = [x | x <- [1..(div n 2)], mod n x == 0] ++ [n]

nonprime :: Int -> Bool
nonprime n = factors n /= [1,n]

process :: [Op] -> [String]
process ops = map show [muls, length nonprimes]
    where (_,_,_,muls) = run (ops, 0, [], 0)
          -- Step 2: Run first 10 instructions to set up b and c.
          (_,_,reg,_) = run (take 10 ops, 0, [('a',1)], 0)
          start = readReg reg 'b'
          end = readReg reg 'c'
          nonprimes = [x | x <- [start,(start+17)..end], nonprime x]

-- long file, lets do IO
main :: IO ()
main = interact (unlines . process . map (parse.words) . lines)

{-
Annotated input:

set b 67            ; b = 67
set c b             ; c = 67
jnz a 2             ; if (a != 0) goto bigger
jnz 1 5             ; goto loop
mul b 100           ; big: b *= 100
sub b -100000       ;      b += 100000
set c b             ;      c = b
sub c -17000        ;      c += 17000
set f 1             ; loop: f = 1
set d 2             ;       d = 2
set e 2             ;    loop2: e = 2
set g d             ;        loop3: g = d
mul g e             ;               g *= e
sub g b             ;               g -= b
jnz g 2             ;               if (g != 0) goto keepF
set f 0             ;                   f = 0
sub e -1            ;       keepF:  e += 1
set g e             ;               g = e
sub g b             ;               g -= b
jnz g -8            ;               if (g != 0) goto loop3
sub d -1            ;           d += 1
set g d             ;           g = d
sub g b             ;           g -= b
jnz g -13           ;           if (g != 0) goto loop2
jnz f 2             ;       if (f != 0) goto skipH
sub h -1            ;       h += 1
set g b             ;skipH: g = b
sub g c             ;       g -= c
jnz g 2             ;       if (g != 0) goto incB
jnz 1 3             ;       goto end
sub b -17           ; incB: b += 17
jnz 1 -23           ;       goto loop
                    ; end:

C version:

#include <stdio.h>
#include <stdint.h>

int main() {
    int64_t b,c,d,e,f,g,h;

    b = 106700;
    c = 123700;
    h = 0;

    do {
        f = 1;
        d = 2;
        do {
            e = 2;
            do {
                if (d * e == b) {
                    f = 0;
                    printf("b %ld = %ld * %ld\n", b,d,e);
                }
                e++;
            } while (e != b);
            d++;
        } while (d != b);
        if (f == 0) {
            printf("inc H!\n");
            h++;
        }
        g = b;
        if (b != c) {
            b += 17;
            printf("next B\n");
        }
    } while (g != c);

    printf("h=%ld\n", h);
    return 0;
}

-}
