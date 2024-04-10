"""
Generate Java solutions for given approaches and models

Usage: python3 generate.py approaches models

   both approaches and models are comma-separated strings

Following Approaches are not valid solutions:
   oo                 -- baseline for comparison. A straight OO solution
   visitor            -- visitor approach
   visitorSideEffect  -- visitor without Generics stores state that is retrieved upon completion of accept method
   dispatch           -- uses casts that can lead to runtime exceptions

Following Approaches offer solutions with different pros and cons

   extensibleVisitor
   interpreter
   trivially
   coco
   algebra

Following Approach generates SVG images for a model
   graphviz
"""

