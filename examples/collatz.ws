--Func
 	 		 			  			 	 	 		 		 	 	 					 			     			  	  		 				 		 		 	 			     			 	  
end----Push_0   
end----Push_-1  		
end----Store		 end----Drop 

end----Drop 

end----Func
 	 		   		 		 				 		 		   		 		   		    	 			 	   				 	 
end----Push_0   
end----Retrieve			end----PrintNum	
 	end----Push_10   	 	 
end----PrintChar	
  end----Exit


end----Mark
   		   		 		 				 		 		   		 		   		    	 			 	   				 	 
end----Func
 	 		 	  	 		 			  		   		 			  	  		  	 	 		 		 	 		  	 	 		 			  			 	   	 					 		   		 		 				 			 	 	 		 			  			 	   		  	 	 			  	 
end----Dup 
 end----Push_1   	
end----Sub	  	end----JumpNeg
		 		   		 		 				 		 		   		 		   		    	 			 	   				 	  	 					 		 			  		 				 	 					 			  	  		  	 	 			 	   			 	 	 			  	  		 			 
end----Return
	
end----Mark
   		   		 		 				 		 		   		 		   		    	 			 	   				 	  	 					 		 			  		 				 	 					 			  	  		  	 	 			 	   			 	 	 			  	  		 			 
end----Drop 

end----Dup 
 end----Push_2   	 
end----Swap 
	end----Mod	 		end----JumpZero
	  		   		 		 				 		 		   		 		   		    	 			 	   				 	  	 					 		  	 	 			 		  		  	 	 		 			 
end----Drop 

end----Push_3   		
end----Mult	  
end----Push_1   	
end----Add	   end----Jump
 
 		   		 		 				 		 		   		 		   		    	 			 	   				 	 
end----Mark
   		   		 		 				 		 		   		 		   		    	 			 	   				 	  	 					 		  	 	 			 		  		  	 	 		 			 
end----Drop 

end----Push_2   	 
end----Swap 
	end----Div	 	 end----Jump
 
 		   		 		 				 		 		   		 		   		    	 			 	   				 	 
end----Mark
   		 			  			 	 	 		 		 	 	 					 			     			  	  		 				 		 		 	 			     			 	  
end----Push_32   	     
end----Push_58   			 	 
end----Push_114   			  	 
end----Push_101   		  	 	
end----Push_98   		   	 
end----Push_109   		 		 	
end----Push_117   			 	 	
end----Push_78   	  			 
end----Dup 
 end----Mark
   	 					 		 			  			 	 	 		 		 	 	 					 			     			  	  		 				 		 		 	 			     			 	   	 					 		 	  	 		 			  		 			  		  	 	 			  	  	 					
end----Drop 

end----PrintChar	
  end----Push_32   	     
end----Sub	  	end----JumpNeg
		 	 					 		 			  			 	 	 		 		 	 	 					 			     			  	  		 				 		 		 	 			     			 	   	 					 		 	  	 		 			  		 			  		  	 	 			  	  	 					
end----Drop 

end----Push_1   	
end----ReadNum	
		end----Retrieve			end----Swap 
	end----Drop 

end----Return
	
end----Mark
   		 	  	 		 			  		   		 			  	  		  	 	 		 		 	 		  	 	 		 			  			 	   	 					 		   		 		 				 			 	 	 		 			  			 	   		  	 	 			  	 
end----Push_0   
end----Retrieve			end----Push_1   	
end----Add	   end----Store		 end----Drop 

end----Drop 

end----Return
	
end--