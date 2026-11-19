# Equals Case Study

This standalone example demonstrates how to extend CoGen to generate arbitrary classes that can then be constructed
using Reify.

CoGen has several built-in TypeReps to deal with the vast majority of primitive types in programming languages, such as TypeRep.Int 
or TypeRep.Boolean. What if wanted to extend to include new types, such as Point?

## Definitions

`DomainModel` contains definitions of `CompositeDataType` which is a recursive structure allowing one to define a 
composite type by name, which contains a sequence of fields, each of which can be primitive `TypeRep` or a composite type.

```
/** Represents a composite data type, composed either from built-in data types, or other composite data types. */
case class CompositeDataType(val name:String, fields:Seq[(String, TypeRep)]) {
  def inst(args: Seq[(String, InstanceRep)]): InstanceRep = InstanceRep(BaseType.CompositeTpe(this))(args)
}
```

Below we define a `Point` class and a `Rectangle` class. To make this definition interesting, the rectangle is defined
to include an array of points as `anchors`.

```
  val point: CompositeDataType = CompositeDataType(
    name = "Point",
    fields = Seq("x" -> TypeRep.Int, "y" -> TypeRep.Int)
  )
  val pointTypeRep: TypeRep.OfHostType[Seq[(String, InstanceRep)]] = BaseType.CompositeTpe(point)
  val pointArray = TypeRep.Array[Seq[(String, InstanceRep)]](pointTypeRep)

  val rectangle: CompositeDataType = CompositeDataType(
    name = "Rectangle",
    fields = Seq(
      "height" -> TypeRep.Int,
      "width" -> TypeRep.Int,
      "anchors" -> pointArray
    )
  )
```

We can define some instances:

```
val pt0 = Seq("x" -> InstanceRep(TypeRep.Int)(1), "y" -> InstanceRep(TypeRep.Int)(2))
val pt1 = Seq("x" -> InstanceRep(TypeRep.Int)(1), "y" -> InstanceRep(TypeRep.Int)(2))
```

We can define some test cases:

```
 val testCases = Seq(
    EqualsTestCase(point.inst(pt0), point.inst(pt1), true),
    ...
 )
```

## Generating test cases

```
for {
  allTests <- forEach(testCases) { test => for {
      objectType <- toTargetLanguageType(test.object1.tpe)
      obj1 <- reify(test.object1)
      obj2 <- reify(test.object2)
      assertion <- if (test.expected) {
        asserts.assertionCapabilities.assertEquals(objectType, obj1, obj2)
      } else {
        asserts.assertionCapabilities.assertNotEquals(objectType, obj1, obj2)
      }

    } yield assertion
  }
} yield allTests
```

This code is quite simple, since it delegates the `reify` capability to the extended CoGen.  

## Extending CoGen

We start by enhancing the `equals.ffi.BaseType` with two new `TypeRep` constants. The `AnyTpe` handles the generic "Object" such 
as would be `java.lang.Object`. The `CompositeTpe` maps to any newly defined class definition.

```
object BaseType {
  type WithBase[Ctxt, B <: AnyParadigm] = BaseType[Ctxt] { val base: B }
  
  case object AnyTpe extends TypeRep {
    override type HostType = Any
  }

  case class CompositeTpe(argument:CompositeDataType) extends TypeRep {
    override type HostType = Seq[(String, InstanceRep)]
  }
}
```

### Extending CoGen in Java

The `java.BaseType` class shows how natively the reification and type lookup are modified.

### Extended CoGen in Scala

This requires first constructing an `inbetween` abstraction, that is further made complete in the `scala` package. The CoCo design
pattern demonstrates how to accomplish this. 

## Execution

You can validate the code works by executing `EqualsJavaDirectToDiskMain` which generates code in `target\eql` that looks like the following:

```
package eql;
public class Point {
    protected int x;
    protected int y;

    public Point(int _x, int _y) {
        this.x = _x;
        this.y = _y;
    }

    @Override()
    public boolean equals(Object o) {
        if (o instanceof eql.Point) {
            eql.Point other = ((eql.Point) o);
            if (!(this.x == other.x)) {
                return false;
            }
            if (!(this.y == other.y)) {
                return false;
            }
            return true;
        }
        return false;
    }
}

package eql;
public class Rectangle {
    protected int height;
    protected int width;
    protected Point[] anchors;

    public Rectangle(int _height, int _width, Point[] _anchors) {
        this.height = _height;
        this.width = _width;
        this.anchors = _anchors;
    }

    @Override()
    public boolean equals(Object o) {
        if (o instanceof eql.Rectangle) {
            eql.Rectangle other = ((eql.Rectangle) o);
            if (!(this.height == other.height)) {
                return false;
            }
            if (!(this.width == other.width)) {
                return false;
            }
            if (!(java.util.Arrays.deepEquals(this.anchors, other.anchors))) {
                return false;
            }
            return true;
        }
        return false;
    }
}
```

You can validate the code works by executing `EqualsScalaDirectToDiskMain` which generates code in `target\eql` that looks like the following:

```
package eql

class Point(x_1: Int,y_2: Int) {
  var x: Int = x_1
  var y: Int = y_2

  override def equals(o: Any): Boolean = {
    if (o.isInstanceOf[eql.Point]) {
      var other_3: eql.Point = o.asInstanceOf[eql.Point]

      if ((!(this.x == other_3.x))) { return false } 
      if ((!(this.y == other_3.y))) { return false } 

      return true 
    } 

    return false
  }
}

package eql

class Rectangle(height_1: Int, width_2: Int, anchors_3: Array[eql.Point]) {
  var height: Int = height_1
  var width: Int = width_2
  var anchors: Array[eql.Point] = anchors_3

  override def equals(o: Any): Boolean = {
    if (o.isInstanceOf[eql.Rectangle]) {

      var other_4: eql.Rectangle = o.asInstanceOf[eql.Rectangle]

      if ((!(this.height == other_4.height))) {
        return { false }
      } 

      if ((!(this.width == other_4.width))) {
        return { false }
      } 

      if (
        (!(this.anchors.corresponds(other_4.anchors)(
            (x_5: eql.Point,y_6: eql.Point) => { x_5 == y_6 }
          )))
      ) {
        return { false }
      } 

      return { true }
    } else {}

    return { false }
  }
}
```