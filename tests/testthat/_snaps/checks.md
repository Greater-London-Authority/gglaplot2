# the palettes' reports are the baseline

    Code
      print(gla_check_palette(gla_pal()), right = FALSE)
    Output
        check               result
      1 Lightness band      fail  
      2 Chroma floor        fail  
      3 CVD separation      warn  
      4 Normal-vision floor pass  
      5 Contrast on surface warn  
        detail                                                                       
      1 outside 0.43 to 0.77: #460d53 (0.297), #fff200 (0.941)                       
      2 below 0.10, reads grey: #57b8a8 (0.095)                                      
      3 worst adjacent pair #eb001b and #41a362, 7.7 (deutan); tritan 19.5           
      4 worst adjacent pair #9e0059 and #f36428, 26.2                                
      5 below 3:1 on #ffffff, needs labels or a table: #57b8a8 (2.38), #fff200 (1.17)

---

    Code
      print(gla_check_palette(gla_pal(theme = "dark"), theme = "dark"), right = FALSE)
    Output
        check               result
      1 Lightness band      fail  
      2 Chroma floor        fail  
      3 CVD separation      warn  
      4 Normal-vision floor fail  
      5 Contrast on surface pass  
        detail                                                                                  
      1 outside 0.48 to 0.67: #00aeef (0.708), #f36428 (0.676), #7cd8c9 (0.821), #fff200 (0.941)
      2 below 0.10, reads grey: #7cd8c9 (0.091)                                                 
      3 worst adjacent pair #eb001b and #41a362, 7.7 (deutan); tritan 4.9                       
      4 worst adjacent pair #da4476 and #f36428, 13.4                                           
      5 all 8 at or above 3:1 on #0e1316                                                        

---

    Code
      print(gla_check_palette(gla_pal(n = 3, theme = "dark"), theme = "dark"), right = FALSE)
    Output
        check               result
      1 Lightness band      fail  
      2 Chroma floor        pass  
      3 CVD separation      pass  
      4 Normal-vision floor fail  
      5 Contrast on surface pass  
        detail                                                            
      1 outside 0.48 to 0.67: #00aeef (0.708), #f36428 (0.676)            
      2 all 3 at or above 0.10                                            
      3 worst adjacent pair #da4476 and #f36428, 12.5 (deutan); tritan 4.9
      4 worst adjacent pair #da4476 and #f36428, 13.4                     
      5 all 3 at or above 3:1 on #0e1316                                  

---

    Code
      print(gla_check_palette(gla_pal("sex")), right = FALSE)
    Output
        check               result
      1 Lightness band      fail  
      2 Chroma floor        pass  
      3 CVD separation      pass  
      4 Normal-vision floor pass  
      5 Contrast on surface pass  
        detail                                                             
      1 outside 0.43 to 0.77: #460d53 (0.297), #460d53 (0.297)             
      2 all 4 at or above 0.10                                             
      3 worst adjacent pair #460d53 and #41a362, 37.4 (deutan); tritan 38.6
      4 worst adjacent pair #460d53 and #41a362, 42.9                      
      5 all 4 at or above 3:1 on #ffffff                                   

---

    Code
      print(gla_check_ramp(gla_pal("sequential", 5)), right = FALSE)
    Output
        check                result detail                        
      1 Lightness monotone   pass   steps run light to dark       
      2 Step gap             pass   every step at least 0.06 apart
      3 Palest step contrast fail   #e6f2fd at 1.14:1 on #ffffff  
      4 Single hue           pass   hue spread 8 degrees          

---

    Code
      print(gla_check_ramp(gla_pal("sequential", 5, theme = "dark"), theme = "dark"),
      right = FALSE)
    Output
        check                result detail                        
      1 Lightness monotone   pass   steps run light to dark       
      2 Step gap             pass   every step at least 0.06 apart
      3 Palest step contrast fail   #001f42 at 1.13:1 on #0e1316  
      4 Single hue           pass   hue spread 5 degrees          

---

    Code
      print(gla_check_ramp(div[1:4]), right = FALSE)
    Output
        check                result
      1 Lightness monotone   pass  
      2 Step gap             fail  
      3 Palest step contrast fail  
      4 Single hue           fail  
        detail                                            
      1 steps run light to dark                           
      2 steps closer than 0.06: #e99890 to #bdc0c2 (0.047)
      3 #bdc0c2 at 1.83:1 on #ffffff                      
      4 hue spread 149 degrees                            

---

    Code
      print(gla_check_ramp(div[4:7]), right = FALSE)
    Output
        check                result
      1 Lightness monotone   pass  
      2 Step gap             fail  
      3 Palest step contrast fail  
      4 Single hue           pass  
        detail                                            
      1 steps run light to dark                           
      2 steps closer than 0.06: #bdc0c2 to #82b3e1 (0.057)
      3 #bdc0c2 at 1.83:1 on #ffffff                      
      4 hue spread 14 degrees                             

