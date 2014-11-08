//Name: Nicholas Noto
//CruzID: nnoto

object Rc4 {
       //keyscheduling algorithm
       def ksa(state:Array[Int], key:Array[Int], keyLength:Int) : Array[Int] ={
       	   var i = 0;
           var j = 0;
           var t = 0;
	   var keylen = key.length;
           for ( i <- 0 until 256){
                state(i) = i;

           }

           for( i <- 0 until 256){
           	 j = (j + state(i) + key(i % keylen)) % 256;
            	 t = state(i);
            	 state(i) = state(j);
            	 state(j) = t;	 
           }

	   var state2 = state.clone();
	   return state2;
       }
       //Pseudo random generator algorithm
       def prga(state:Array[Int], output:Array[Int], keyLength:Int) : Array[Int] ={
       	   var j = 0;
	   var k = 0;
 	   var t = 0;
	   for (i <- 0 until keyLength){
	       k = (k + 1) % 256;
	       j = (j + state(k)) % 256;
	       t = state(k);
	       state(k) = state(j);
	       state(j) = t;
	       output(i) = state((state(k) + state(j)) % 256);
	   }
	   var output2 = output.clone();
       	   return output2;
       }
       def main(args: Array[String]) {
       	   var i = 0;
       	   var state1 = new Array[Int](256);
       	   var stream = new Array[Int](1024);
       	   var plaintext = "Plaintext";
	       println("Plaintext:");
	       println(plaintext);
	       var plaintext1 = plaintext.toCharArray();
	       var keyLength = 16;
	       var key = "Key";
       	   var key1 = new Array[Int](key.length()); 
	       for (i <- 0 until key.length()){
	           key1(i) = ((key.charAt(i)).toInt - 48)
	       }
	       var state3 = ksa(state1, key1, keyLength);
	       var stream2 = prga(state3, stream, keyLength);
	       println("Encrypted: ");
           for (i <- 0 until plaintext.length()){
	           var x = stream2(i);
	           plaintext1(i) = (plaintext1(i).toInt ^ x).toChar;
	           print(plaintext1(i));
	       }	    
	       println("");
	       println("Decrypted: ");
	       for (i <- 0 until plaintext.length()){
	           var x = stream2(i);
	           plaintext1(i) = (plaintext1(i).toInt ^ x).toChar;
	           print(plaintext1(i));
	       }
	       println("");
        }   
}