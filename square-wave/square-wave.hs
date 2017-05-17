main=mapM_(\i->(putStr.(\s->s++replicate(4-length s)' ').show)i>>(print.f)i)[-19..19]

-- | Actual golf material:
f x=abs(mod(x-1)4-2)-1
