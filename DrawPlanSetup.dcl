DrawPlanSetup : dialog {		    //dialog name
   label = "Draw Plan Setup" ;    //label (visible to user)
 
   : row {					//define row
   
      : text {label = "Post Spacing";}
      
		: edit_box {key = postSpacingBox; width = 4; value = "48";}
  
      :boxed_radio_column {		      	//define radio column
         label = "Top Rail Type" ;      	//label
    
         : radio_button {			         //define radion button
            key = "s200Button" ;       	//name
            label = "Series &200" ;      	//label
            value = "1" ;				      //switch it on
         }	   				               //end definition
    
         : radio_button {			
            key = "s200xButton" ;				
            label = "Series 200&X" ;		
         }					
    
         : radio_button {			
            key = "woodButton" ;				
            label = "Wood Adapter" ;		
         }					
         
         : radio_button {			
            key = "s100Button" ;				
            label = "Series 100" ;	
         }
      }
      
      
   }
        
   ok_cancel ;				                  //predifined OK/Cancel
     					
}