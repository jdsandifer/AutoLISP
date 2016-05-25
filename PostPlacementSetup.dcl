PostPlacementSetup : dialog {				//dialog name
   label = "Post Placement Setup" ;    //label (visible to user)
 
   : row {					//define row
  
      :boxed_radio_column {		      	//define radio column
         label = "Mounting Type" ;      	//label
    
         : radio_button {			         //define radion button
            key = "surface" ;			     	//name
            label = "&Surface" ;        	//label
            value = "1" ;				      //switch it on
         }	   				               //end definition
    
         : radio_button {			
            key = "fascia" ;				
            label = "&Fascia" ;		
         }					
    
         : radio_button {			
            key = "stanchion" ;				
            label = "S&tanchion" ;		
         }					
         
         : radio_button {			
            key = "core" ;				
            label = "&Core" ;	
         }
      }
      
      :column {
      
      :boxed_radio_column {		      	
         label = "Post Layer" ;      	
    
         : radio_button {			         
            key = "detail" ;			     	
            label = "\"&Detail\"" ;        	
            value = "1" ;				      
         }	   				               
    
         : radio_button {			
            key = "hral-post" ;				
            label = "\"A-HRAL-POST\"" ;		
         }					
      }
      
      :boxed_radio_column {		      	
         label = "Draw Cables/Tags" ;      	
    
         : radio_button {			         
            key = "cable" ;			     	
            label = "&Yes" ;        	
            value = "1" ;				      
         }	   				               
    
         : radio_button {			
            key = "noCable" ;				
            label = "&No" ;		
         }					
      }}
   }
        
   ok_cancel ;				                  //predifined OK/Cancel
     					
}