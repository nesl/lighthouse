Current checker does not catch this bug.  The generated code from this
function looks like:

---
#line 24
      __cil_tmp4 = (unsigned int )msg;
      __cil_tmp5 = __cil_tmp4 + 4;
      __cil_tmp6 = (*((void **)__cil_tmp5));
      ker_change_own(__cil_tmp6, pid);
#line 25
      __cil_tmp7 = (unsigned int )msg;
      __cil_tmp8 = __cil_tmp7 + 4;
      __cil_tmp9 = (*((void **)__cil_tmp8));
      ret = (int *)__cil_tmp9;
#line 26
      (*((int *)msg)) = 0;
#line 27
      __cil_tmp10 = (unsigned int )msg;
      __cil_tmp11 = __cil_tmp10 + 4;
      (*((void **)__cil_tmp11)) = (void *)0;
  }
#line 28
  return ((void *)ret);
----

The question is if ret is full when it hits the return statement at line 28.
This is true from inspection.  In particular:

ret = tmp9 = tmp8 = tmp7+4 = msg+4 = tmp4+4 = tmp5 = tmp6 = heap

I am guessing that the is equivalent analysis is unable to reason about this
"down and back up" transition that is needed to correlate tmp7+4 with
tmp4+4.

Bummer.
