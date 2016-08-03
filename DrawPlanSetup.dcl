DrawPlanSetup : dialog {		    //dialog name
   label = "Draw Plan Setup" ;    //label (visible to user)
 
   : row {
   
      : column {	
       
         :boxed_radio_column {
            label = "Post Spacing";
            
            : radio_button {			
               key = "spacing48";				
               label = "&48\" (4'-0\")";
               value = 1; }
            
            : radio_button {			
               key = "spacing60";				
               label = "&60\" (5'-0\")"; }
            
            : radio_button {			
               key = "spacing72";				
               label = "&72\" (6'-0\")"; }
         
            : radio_button {			
               key = "spacingOther";				
               label = "&Other:"; }
            
            : edit_box {
               key = spacing; 
               width = 1; 
               value = "36"; }}
         
         :boxed_radio_column {
            label = "Intermediate Post Type";
            
            : radio_button {			
               key = "postFascia" ;				
               label = "Fascia, Std. Post"; }
            
            : radio_button {			
               key = "postSurface" ;				
               label = "Surface, Std. Post";
               value = 1; }	
                        
            : radio_button {			
               key = "postFasciaTrimline";				
               label = "Fascia, Trimline"; }
         
            : radio_button {			
               key = "postSurfaceTrimline";				
               label = "Surface, Trimline"; }}}
            
      :boxed_radio_column {		      	//define radio column
         label = "Top Rail Type";      	//label
         
         : radio_button {			         //define radion button
            key = "s100";            	   //name
            label = "Series &100";      	//label
         }
         
         : radio_button {			
            key = "s200";				
            label = "Series &200"; }	   				               
    
         : radio_button {			
            key = "s200X";				
            label = "Series 200&X";
            value = "1"; }			         //switch it on					
    
         : radio_button {			
            key = "s300";				
            label = "Series &300"; }	   				               
    
         : radio_button {			
            key = "s300X";				
            label = "&Series 300X"; }
         
         : radio_button {			
            key = "s320X";				
            label = "S&eries 320X"; }
         
         : radio_button {			
            key = "s350X";				
            label = "Se&ries 350X"; }
         
         : radio_button {			
            key = "s500";				
            label = "Series &500"; }
         
         : radio_button {			
            key = "woodAdapter";				
            label = "&Wood Adapter"; }}}
        
   ok_cancel; }				                  //predifined OK/Cancel