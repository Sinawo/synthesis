module Synthesis

open System
open System.Linq.Expressions

let abelar a  =
    a > 12 && a < 3097 && a%12=0



let area b h =
       match b >= 0.0 && h >= 0.0 with
       | true -> 0.5* b* h 
       | _ -> failwith "fails"
       

let zollo a =
     match (a >= 0) with 
     | true -> a*2
     | _ -> (-a)
 

let min a b =
      match a< b with
      |true -> a 
      | _ -> b
    

let max  a b =
      match a> b with
      |true -> a 
      | _ -> b

let ofTime h m s =
          h*3600 + m*60 + s

let toTime time = 
       match time>=0 with
    |false -> 0,0,0
    |true -> let hour=time/3600
             let minutes= (time - hour*3600)/60
             let sec = (time- (hour*3600) - minutes*60)
             hour,minutes,sec
          
                      
       
           
      

let digits a =
          let rec count c acc =
             match c = 0 with
             |true -> acc
             | false -> count (c/10)  acc+1
          count a 0
           
           
        
    

let minmax (a,b,c,d) = // //4,6,8,10   1,3,2,4
                     
        
             let minV =  min a b |> min c|>min d
             let maxV =  max a b |> max c|>max d
             minV,maxV
                  
      
       
let isLeap year =
     
              match year < 1582  with
              | true -> failwith "number supplied is less than 1582"
              | _ -> 
                    match (   ((year % 4 = 0) || (year % 400 = 0)) && year % 100 <> 0) with
                    | true
                    | _ -> false
                 
                    
    
          


    

let month day =
             match  day>= 1 && day <= 12 with
             | true -> match day with
                      | 1 -> "January" ,31
                      | 2  -> "February", 28
                      | 3 ->"March", 31
                      | 4 -> "April", 30
                      | 5 -> "May", 31
                      |6 -> "June", 30
                      |7 -> "July", 31
                      | 8 -> "August", 31
                      |9  -> "September", 30
                      |10 -> "October", 31
                      |11 -> "November", 30
                      |12 -> "December", 31

             |false -> failwith "your nmonth number is not valid"

let toBinary num =
        let rec biNary digit str=
         match digit<0 with
         |true->failwith "Negative digit"
         |_->match digit=0 && str<>"" with
            |true->str
            |false-> match digit%2<>0 with
                     |true->biNary (digit/2) ("1"+str)
                     |_->biNary (digit/2) ("0"+str)
        biNary num "0"
          
    

let bizFuzz _ =
    failwith "Not implemented"

let monthDay _ _ =
            
    failwith "Not implemented"

let coord _ =
    failwith "Not implemented"