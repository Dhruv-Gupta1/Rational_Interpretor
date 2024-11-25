signature BIGINT = 
    sig
        type bigint
        exception divide_by_zero_error
        val from_string: string -> bigint
        val trim: bigint -> bigint
        val to_string: bigint -> string
        val add: bigint -> bigint -> bigint
        val add1: bigint -> bigint -> bigint
        val subtract: bigint -> bigint -> bigint
        val multiply: bigint -> bigint -> bigint
        val geq: bigint -> bigint -> bool
        val test_div: bigint * bigint -> bigint
        val div_step: bigint * bigint * bigint * bigint -> bigint * bigint
        val divmod: bigint -> bigint -> bigint * bigint
        val gcd : bigint -> bigint -> bigint
        val hd : bigint -> int
        val tl : bigint -> bigint
        val is_empty : bigint -> bool
        val preappendneg1 : bigint -> bigint
        val create_example : int list -> bigint
        val equal : bigint -> bigint -> bool
        val append : bigint -> bigint -> bigint
        val create_nines : bigint -> bigint
    end;

structure Bigint : BIGINT = struct
    type bigint = int list
    exception divide_by_zero_error
    (*from_string : converts string to bigint type number*)
    fun from_string s = 
        let
            val x = String.explode s
            fun from_string1 [] = []
                | from_string1 (x::xs) = 
                    if(x<> #"+" andalso x<> #"~") then (ord(x)-ord(#"0"))::(from_string1 xs)
                    else if(x = #"~") then (~1)::(from_string1 xs)
                    else from_string1 xs
        in
            from_string1 x
        end
    (*equal : checks if 2 numbers are equal*)
    fun equal x y = (x=y)
    (*trim : rims the first zero part(if exists) in bigint number represented in int list*)
    fun trim [] = []
        | trim (x::xs) = if (x=0) then trim xs
                         else if (x= ~1) then ~1:: trim (xs)
                         else x::xs
    (*to_string : Converts positive unsigned bigint number to string*)
    fun to_string [] = ""
        | to_string (~1::xs) = "~" ^ to_string xs
        | to_string (x::xs) = Int.toString x ^ to_string xs 
    (*geq : greater than equal to gives true if x >= y*)
    fun geq x y =
        if (List.length x > List.length y) then true
        else if (List.length x < List.length y) then false
        else
            let
                fun compare [] y = true
                    | compare x [] = false
                    | compare (x1h::x1t) (y1h::y1t) = 
                        if (x1h>y1h) then true
                        else if (x1h<y1h) then false
                        else compare x1t y1t
            in
                compare x y
            end
    (*add1 : Adds 2 positive bigints *)
    fun add1 xs [] = xs
            | add1 [] ys = ys
            | add1 (x::xs) (y::ys) = 
            if(x+y<10) then   (x+y::(add1 xs ys))
            else  ((x+y) mod 10::(add1 (add1 [1] xs) ys))
    (*add : Adds 2 general bigints*)
    fun add xs [] = xs
        | add [] ys = ys
        | add (x::xs) (y::ys) = 
        let 
            val x1 = rev (x::xs)
            val y1 = rev (y::ys)
            fun add1 xs [] = xs
            | add1 [] ys = ys
            | add1 (x::xs) (y::ys) = 
            if(x+y<10) then   (x+y::(add1 xs ys))
            else  ((x+y) mod 10::(add1 (add1 [1] xs) ys))
            fun subtract1 xs [] = xs
            | subtract1 [] ys = []
            | subtract1 (x::xs) (y::ys) =
            if(x-y>=0) then x-y::(subtract1 xs ys)
            else (x-y+10)::(subtract1 (subtract1 xs [1]) ys)
        in
            if(x= ~1 andalso y= ~1) then ~1::(trim (rev (add1 (rev xs) (rev ys))))
            else if(x= ~1 andalso (geq xs (y::ys)) ) then ~1::(trim (rev (subtract1 (rev xs) y1)))
            else if(x= ~1) then (trim (rev (subtract1 y1 (rev xs))))
            else if(y= ~1 andalso (geq ys (x::xs)) ) then ~1::(trim (rev (subtract1 (rev ys) x1)))
            else if(y= ~1) then (trim (rev (subtract1 x1 (rev ys))))
            else (trim (rev (add1 x1 y1))) 
        end
    (*subtract x y : Subtract y from x both bigints*)
    fun subtract xs [] = xs
        | subtract [] (y::ys) = if(y= ~1) then (ys)
                                else ~1::(y::ys)
        | subtract (x::xs) (y::ys) = 
        let 
            val x1 = rev (x::xs)
            val y1 = rev (y::ys)
            fun add1 xs [] = xs
            | add1 [] ys = ys
            | add1 (x::xs) (y::ys) = 
            if(x+y<10) then   (x+y::(add1 xs ys))
            else  ((x+y) mod 10::(add1 (add1 [1] xs) ys))
            fun subtract1 xs [] = xs
            | subtract1 [] ys = []
            | subtract1 (x::xs) (y::ys) =
            if(x-y>=0) then x-y::(subtract1 xs ys)
            else (x-y+10)::(subtract1 (subtract1 xs [1]) ys)
        in
            if(x= ~1 andalso y= ~1 andalso (geq xs ys)) then ~1::(trim (rev (subtract1 (rev xs) (rev ys))))
            else if(x= ~1 andalso y= ~1) then (trim (rev (subtract1 (rev ys) (rev xs))))
            else if(x= ~1) then ~1::(trim (rev (add1 y1 (rev xs))))
            else if(y= ~1) then (trim (rev (add1 x1 (rev ys))))
            else if (geq (x::xs) (y::ys)) then  (trim (rev (subtract1 x1 y1))) 
            else ~1::(trim (rev (subtract1 y1 x1)))
        end
    (*multiply_h : helper function for multiply (multiply positive bigint numbers)*)
    fun multiply_h xs [] = []
        | multiply_h [] ys = []
        | multiply_h [x] (y::ys) =
            if(x*y<10) then x*y::(multiply_h [x] ys)
            else (x*y) mod 10::(add1 [x*y div 10] (multiply_h [x] ys))
        | multiply_h (x::xs) [y] = 
            if(x*y<10) then x*y::(multiply_h xs [y])
            else (x*y) mod 10::(add1 [x*y div 10] (multiply_h xs [y]))
        | multiply_h (x::xs) y =
            add1 (multiply_h [x] y) (0::(multiply_h xs y))
    (*multiply : multiply 2 bigints*)
    fun multiply [] x = []
        | multiply x [] = []
        |multiply (x::xs) (y::ys) = 
        if(x= ~1 andalso y= ~1) then (rev (multiply_h (rev xs) (rev ys)))
        else if(x= ~1) then ~1::(rev (multiply_h (rev xs) (rev (y::ys))))
        else if(y= ~1) then ~1::(rev (multiply_h (rev (x::xs)) (rev ys)))
        else (rev (multiply_h (rev (x::xs)) (rev (y::ys))))
    (*test_div : helper fun for division gives least max single digit j such that number * j >divi *)
    fun test_div(divi,number) = 
        let
            fun helper(divi,number,vl) = 
                if (geq number (multiply divi vl)) then vl
                else helper(divi,number,subtract vl [1])
        in
            helper(divi,number,[9])
        end
    (*div_step : generalised division helper function*)
    fun div_step(divisor,[],first,q) = 
        if(geq first divisor) then
            let
                val next_q_bit = test_div(divisor,first)
                val next_first = subtract first (multiply divisor next_q_bit) 
                (*val next_first = Bigint.subtract first (Bigint.multiply divisor next_q_bit) *)
            in
                if(next_q_bit = []) then div_step(divisor,[],next_first,q@[0])
                else div_step(divisor,[],next_first,q@next_q_bit)
                
            end
        else
            (q,first)
        | div_step(divisor,c::rest,first,q) =
            if (geq first divisor) then
                let
                    val next_q_bit = test_div(divisor,first)
                    val next_first = subtract first (multiply divisor next_q_bit) 
                in
                    if(next_q_bit = []) then div_step(divisor,c::rest,next_first,q@[0])
                    else div_step(divisor,c::rest,next_first,q@next_q_bit)
                end
            else if (c <> 0) then
                let
                    val next_q_bit = test_div(divisor,first@[c])
                    (*val next_q_bit = Bigint.test_div(divisor,first@[c])*)
                    val next_first = subtract (first@[c]) (multiply divisor next_q_bit) 
                    (*val next_first = Bigint.subtract (first@[c]) (Bigint.multiply divisor next_q_bit) *)
                in
                    if(next_q_bit = []) then div_step(divisor,rest,next_first,q@[0])
                    else div_step(divisor,rest,next_first,q@next_q_bit)
                end
            else
                let
                    val next_q_bit = test_div(divisor,trim(first@[c]))
                    (*val next_q_bit = Bigint.test_div(divisor,first@[c])*)
                    val next_first = subtract (trim(first@[c])) (multiply divisor next_q_bit) 
                    (*val next_first = Bigint.subtract (first@[c]) (Bigint.multiply divisor next_q_bit) *)
                in
                    if(next_q_bit = []) then div_step(divisor,rest,next_first,q@[0])
                    else div_step(divisor,rest,next_first,q@next_q_bit)
                end
    (*divmod : divide 2 bigints and return quotient and remainder*)
    fun divmod x y = 
        if(y = []) then raise divide_by_zero_error
        else if (equal x y) then ([1],[])
        else if (geq y x) then ([],x)
        else
            let
                val n = List.length y
                val first = List.take(x,n)
                val rest =  List.drop(x,n)
            in
                div_step(y,rest,first,[])
            end
    (*gcd : find gcd of 2 bigints*)
    fun gcd [] y = y
        | gcd x [] = x
        | gcd x y = 
        if (geq x y) then
            let
                val (q,r) = divmod x y
            in
                if (r = []) then y
                else gcd y r
            end
        else gcd y x
    (*hd : gives head of the int list*)
    fun hd [] = 0
        | hd (x::xs) = x
    (*tl : gives tail of the int list*)
    fun tl [] = []
        | tl (x::xs) = xs
    (*is_empty : checks if the int list is empty or not*)
    fun is_empty [] = true
        | is_empty (x::xs) = false
    (*preappendneg1 : preappend -1 to the int list*)
    fun preappendneg1  ys = ~1::ys
    (*create_example : create bigints from int list sort of identity*)
    fun create_example x = x
    (*append : append 2 int lists in reverse order*)
    fun append x y = y@x
    (*create_nines : create a list of nines of the same length as the int list*)
    fun create_nines [] =[]
        | create_nines (x::xs) = 9::(create_nines xs)
    end

signature RATIONAL =
    sig
        type bigint
        type rational
        exception rat_error
        val make_rat: bigint * bigint -> rational option
        val neg: rational -> rational
        val rat: bigint -> rational option
        val reci: bigint -> rational option
        val inverse : rational -> rational option
        val equal : rational * rational -> bool 
        val less : rational * rational -> bool 
        val add : rational * rational -> rational 
        val subtract : rational * rational -> rational 
        val multiply : rational * rational -> rational 
        val divide : rational * rational -> rational option 
        val showRat : rational -> string
        val seperator : string -> bigint * bigint * bigint
        val fromDecimal : string -> rational
        val showDecimal : rational -> string
        val toDecimal : rational -> string
    end;



functor Rational(Bigint:BIGINT) : RATIONAL = 
struct
    type bigint = Bigint.bigint
    type rational = bigint * bigint
    exception rat_error
    (*make_rat : make a rational number from 2 bigints*)
    fun make_rat (n,d) = 
        if(Bigint.hd(n) = ~1 andalso Bigint.hd(d) = ~1) then 
            let
                val n1 = Bigint.tl(n)
                val n2 = Bigint.tl(d)
                val n3 = Bigint.gcd n1 n2  
                val a1= #1 (Bigint.divmod n1 n3 )
                val a2= #1 (Bigint.divmod n2 n3 )
            in
                SOME (a1,a2)
            end
        else if(Bigint.hd(d) = ~1) then 
            let
                val n1 = Bigint.tl(d)
                val n2 = Bigint.gcd n n1  
                val a1= #1 (Bigint.divmod n n2 )
                val a2= #1 (Bigint.divmod n1 n2 )
            in
                SOME (Bigint.preappendneg1(a1),a2) 
            end
        else if(Bigint.hd(n) = ~1) then
            let
                val n2 = Bigint.tl(n)
                val n1 = Bigint.gcd n2 d  
                val a1= #1 (Bigint.divmod n2 n1 )
                val a2= #1 (Bigint.divmod d n1 )
            in
                SOME (Bigint.preappendneg1(a1),a2)
            end
        else
            let
                val n1 = Bigint.gcd n d  
                val a1= #1 (Bigint.divmod n n1 )
                val a2= #1 (Bigint.divmod d n1 )
            in
                SOME (a1,a2)
            end
    (*neg : negate a rational number*)
    fun neg(q : Bigint.bigint * Bigint.bigint) = 
        let
            val n = #1 q
            val d = #2 q
        in
            if(Bigint.hd(#1 q) = ~1) then (Bigint.tl(#1 q),(#2 q))
            else (Bigint.preappendneg1(#1 q),(#2 q))
        end
    (*rat : make a rational number from a bigint*)
    fun rat( n : Bigint.bigint) = make_rat (n,Bigint.create_example([1]))
    (*reci : make a rational number from a bigint by reciprocal*)
    fun reci( n : Bigint.bigint) = make_rat (Bigint.create_example([1]),n)
    (*is_neg : check if a rational number is negative or not*)
    fun is_neg( q : Bigint.bigint * Bigint.bigint) = 
        if(Bigint.hd(#1 q) = ~1) then true
        else false
    (*inverse : inverse(reciprocal) a rational number*)
    fun inverse( q : Bigint.bigint * Bigint.bigint) = 
        let
            val n = #1 q
            val d = #2 q
        in
            make_rat (d,n)
        end
    (*equal : check if 2 rational numbers are equal or not*)
    fun equal( q1 : Bigint.bigint * Bigint.bigint, q2 : Bigint.bigint * Bigint.bigint) = 
        let
            val n1 = #1 q1
            val d1 = #2 q1
            val n2 = #1 q2
            val d2 = #2 q2
        in
            if((Bigint.equal n1 n2 ) andalso (Bigint.equal d1 d2)) then true
            else false
        end
    (*less : check if 1 rational number is less than the other or not*)
    fun less( q1 : Bigint.bigint * Bigint.bigint, q2 : Bigint.bigint * Bigint.bigint) = 
        if(is_neg(q1) andalso is_neg(q2)) then
            let
                val n1 = Bigint.tl(#1 q1)
                val d1 = Bigint.tl(#2 q1)
                val n2 = Bigint.tl(#1 q2)
                val d2 = Bigint.tl(#2 q2)
            in
                if(Bigint.geq (Bigint.multiply n1 d2) (Bigint.multiply n2 d1)) then true
                else false
            end
        else if(is_neg(q1)) then true
        else if(is_neg(q2)) then false
        else 
            let
                val n1 = #1 q1
                val d1 = #2 q1
                val n2 = #1 q2
                val d2 = #2 q2
            in
                if(Bigint.geq (Bigint.multiply n1 d2) (Bigint.multiply n2 d1)) then false
                else true
            end
    (*add : add 2 rational numbers*)
    fun add( q1 : Bigint.bigint * Bigint.bigint, q2 : Bigint.bigint * Bigint.bigint) =
        let
            val n1 = #1 q1
            val d1 = #2 q1
            val n2 = #1 q2
            val d2 = #2 q2
            val nr =Bigint.add (Bigint.multiply n1 d2) (Bigint.multiply n2 d1)
            val dr =(Bigint.multiply d1 d2)
        in
            case make_rat(nr,dr) of
                SOME x => x
                | NONE => raise rat_error
        end
    (*subtract : subtract 2 rational numbers*)
    fun subtract( q1 : Bigint.bigint * Bigint.bigint, q2 : Bigint.bigint * Bigint.bigint) =
        let
            val n1 = #1 q1
            val d1 = #2 q1
            val n2 = #1 q2
            val d2 = #2 q2
            val nr =Bigint.subtract (Bigint.multiply n1 d2) (Bigint.multiply n2 d1)
            val dr =(Bigint.multiply d1 d2)
        in
            case make_rat(nr,dr) of
                SOME x => x
                | NONE => raise rat_error
        end
    (*multiply : multiply 2 rational numbers*)
    fun multiply( q1 : Bigint.bigint * Bigint.bigint, q2 : Bigint.bigint * Bigint.bigint) =
        let
            val n1 = #1 q1
            val d1 = #2 q1
            val n2 = #1 q2
            val d2 = #2 q2
            val nr =Bigint.multiply n1 n2
            val dr =(Bigint.multiply d1 d2)
        in
            case make_rat(nr,dr) of
                SOME x => x
                | NONE => raise rat_error
        end
    (*divide : divide 2 rational numbers*)
    fun divide( q1 : Bigint.bigint * Bigint.bigint, q2 : Bigint.bigint * Bigint.bigint) =
        let
            val n1 = #1 q1
            val d1 = #2 q1
            val n2 = #1 q2
            val d2 = #2 q2
            val nr =Bigint.multiply n1 d2
            val dr =(Bigint.multiply d1 n2)
        in
            make_rat(nr,dr) 
        end
    (*showRat : show a rational number as a string*)
    fun showRat( q : Bigint.bigint * Bigint.bigint) = 
        let
            val n = #1 q
            val d = #2 q
        in
            if(Bigint.hd(n) = ~1) then
                if(Bigint.equal d (Bigint.create_example([1]))) then
                    ("-"^Bigint.to_string (Bigint.tl(n)))
                else
                    ("-"^(Bigint.to_string (Bigint.tl(n))) ^ "/" ^ (Bigint.to_string d))
            else
                if(Bigint.equal d (Bigint.create_example([1]))) then
                    (Bigint.to_string n)
                else
                    ((Bigint.to_string n) ^ "/" ^ (Bigint.to_string d))
        end
    (*seperator : seperate a decimal number string into 3 parts, the integer part, the non repeating part and the repeating part used as helper funstion to fromDecimal*)
    fun seperator(str : string) = 
        let
        val x = String.explode str
            fun help([],int1 : Bigint.bigint,int2 : Bigint.bigint,int3 : Bigint.bigint,phase : int) = (int1,int2,int3)
                | help(x::xs,int1 : Bigint.bigint,int2 : Bigint.bigint,int3 : Bigint.bigint,phase : int) =
                    if(x = #"+" orelse x = #")") then help(xs,int1,int2,int3,phase)
                    else if(x = #".") then help(xs,int1,int2,int3,2)
                    else if(x = #"(") then help(xs,int1,int2,int3,3)
                    else if(x = #"~" orelse x = #"-") then help(xs,Bigint.preappendneg1(int1),int2,int3,1)
                    else if(phase = 1) then help(xs,Bigint.append (Bigint.create_example([ord(x)-ord(#"0")])) int1,int2,int3,1)
                    else if(phase = 2) then help(xs,int1,Bigint.append (Bigint.create_example([ord(x)-ord(#"0")])) int2,int3,2)
                    else if(phase = 3) then help(xs,int1,int2,Bigint.append (Bigint.create_example([ord(x)-ord(#"0")])) int3,3)
                    else help(xs,int1,int2,int3,phase)
        in
            help(x,Bigint.create_example([]),Bigint.create_example([]),Bigint.create_example([]),1)
        end
    (*fromDecimal : convert a decimal number string into a rational number*)
    fun fromDecimal(str : string) =
        if (Bigint.is_empty(#2 (seperator(str)))  andalso Bigint.is_empty(#3 (seperator(str)))) then
            let 
                val (a : Bigint.bigint,b : Bigint.bigint,c : Bigint.bigint) = seperator(str)
                val a_rat = case rat(a) of 
                            SOME x => x
                            | NONE => raise rat_error
            in
                a_rat
            end
        else if(Bigint.is_empty(#2 (seperator(str)))) then
            let
                val (a : Bigint.bigint,b : Bigint.bigint,c : Bigint.bigint) = seperator(str)
                val a_rat = case rat(a) of 
                            SOME x => x
                            | NONE => raise rat_error
                val c1 = Bigint.create_nines(c)
                val c1_rat = make_rat(c,c1)
                val c_rat = case c1_rat of 
                                SOME x => x
                                | NONE => raise rat_error
                val ans = add(a_rat,c_rat)
            in
                ans
            end
        else if(Bigint.is_empty(#3 (seperator(str)))) then
            let
                val (a : Bigint.bigint,b : Bigint.bigint,c : Bigint.bigint) = seperator(str)
                val a_rat = case rat(a) of 
                            SOME x => x
                            | NONE => raise rat_error
                val b1 = Bigint.create_nines(b)
                val b1 = Bigint.add b1 (Bigint.create_example([1]))
                val b1_rat = make_rat(b,b1)
                val b_rat = case b1_rat of 
                                SOME x => x
                                | NONE => raise rat_error
                val ans = add(a_rat,b_rat)
            in
                ans
            end
        else
            let
                val (a : Bigint.bigint,b : Bigint.bigint,c : Bigint.bigint) = seperator(str)
                val a_rat = case rat(a) of 
                                SOME x => x
                                | NONE => raise rat_error
                val b1 = Bigint.create_nines(b)
                val b1 = Bigint.add b1 (Bigint.create_example([1]))
                val b1_rat = make_rat(b,b1)
                val b_rat = case b1_rat of 
                                SOME x => x
                                | NONE => raise rat_error
                val c1 = Bigint.create_nines(c)
                val c1 = Bigint.multiply c1 b1
                val c1_rat = make_rat(c,c1)
                val c_rat = case c1_rat of 
                                SOME x => x
                                | NONE => raise rat_error
                val ans = add(add(a_rat,b_rat),c_rat)
            in
                ans
            end
    (*showDecimal : convert a rational number into a decimal number string*)
    fun showDecimal( q : Bigint.bigint * Bigint.bigint) = 
        let 
            val n = #1 q
            val d = #2 q
            fun search([x],r) = Bigint.equal x r
                | search((x::xs),r) = if(Bigint.equal x r) then true else search (xs,r)
            fun search_pos([x],r,cnt) = Bigint.create_example([1])
                | search_pos((x::xs),r,cnt) = if(Bigint.equal x r) then cnt else search_pos(xs,r,Bigint.add cnt (Bigint.create_example([1])))
            val int_part = #1 (Bigint.divmod n d)
            val req = Bigint.to_string(int_part)^"."
            val frac_part = #2 (Bigint.divmod n d)
            val all_rems_array = [frac_part]
            fun iterator(frac_part,d,all_rems_array,str) = 
                let
                    val frac_part = Bigint.multiply frac_part (Bigint.create_example([10]))
                    val newpres1 = #1 (Bigint.divmod frac_part d)
                    val frac_part = #2 (Bigint.divmod frac_part d)
                    val part = Bigint.to_string(newpres1)
                in
                    if(search(all_rems_array,frac_part)) then
                        (frac_part,all_rems_array,str^part)
                    else
                        iterator(frac_part,d,all_rems_array @ [frac_part],str^part)
                end
            val itr = iterator(frac_part,d,all_rems_array,"")
            val fr = #1 itr
            val arr = #2 itr
            val st = #3 itr
            val pos = search_pos(arr,fr,Bigint.create_example([1]))
            val stro1 = Bigint.to_string pos
            val SOME final_pos = Int.fromString stro1
            val stro2 = req^substring(st,0,final_pos-1)^"("^substring(st,final_pos-1,size st - final_pos+1)^")"
        
        in
            stro2
        end
    (*toDecimal : convert a rational number into a decimal number string*)
    fun toDecimal( q : Bigint.bigint * Bigint.bigint) = showDecimal(q)
end

structure Rationals = Rational(Bigint)