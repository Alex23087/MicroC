fun fact(n : int) : int = 
  if n = 0 then 
    1
  else
    n * fact (n - 1)
end

fact 5
