# Basic pipeline

```
uuid |> strip "-" |> lower
```

roughly equivalent to:

```
id := uuid.NewString()  
id = strings.Replace(id, "-", "")
strings.Lower(id)
```

# Parser

```
Expr := Call
	| Pipe Call Expr
```

# Types

`currentTime |> toDate`		-- should be fine

`currentTime |> strip "-"`	-- should error

# Errors

`parseTime "rubbish" |> formatTime "YY-mm-dd"` -- pass the "Err" along the pipeline