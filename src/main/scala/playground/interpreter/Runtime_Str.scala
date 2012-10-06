/*
 * Copyright (c) 2012, Oracle and/or its affiliates. All rights reserved.
 * DO NOT ALTER OR REMOVE COPYRIGHT NOTICES OR THIS FILE HEADER.
 *
 * This code is free software; you can redistribute it and/or modify it
 * under the terms of the GNU General Public License version 2 only, as
 * published by the Free Software Foundation.
 *
 * This code is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
 * FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
 * version 2 for more details (a copy is included in the LICENSE file that
 * accompanied this code).
 *
 * You should have received a copy of the GNU General Public License version
 * 2 along with this work; if not, write to the Free Software Foundation,
 * Inc., 51 Franklin St, Fifth Floor, Boston, MA 02110-1301 USA.
 *
 * Please contact Oracle, 500 Oracle Parkway, Redwood Shores, CA 94065 USA
 * or visit www.oracle.com if you need additional information or have any
 * questions.
 */
package playground.interpreter


import java.lang.reflect.{Array=>jlrArray,_};
import java.util._;
import sun.misc._;

import com.oracle.graal.api._;
import com.oracle.graal.api.meta._;
import com.oracle.graal.hotspot.meta._;
import com.oracle.graal.bytecode._;



trait InterpreterUniverse_Str extends Base_Str with InterpreterUniverse {

object unsafe {

  def monitorEnter(value: Rep[Object]): Rep[Unit] = 
    reflect("unsafe.monitorEnter("+value+")")
  def monitorExit(value: Rep[Object]): Rep[Unit] = 
    reflect("unsafe.monitorExit("+value+")")

  def getObject(base: Rep[Object], offset: Rep[Long]): Rep[Object] = 
    reflect("unsafe.getObject("+base+","+offset+")")
  def getObjectVolatile(base: Rep[Object], offset: Rep[Long]): Rep[Object] = 
    reflect("unsafe.getObjectVolatile("+base+","+offset+")")

  def getBoolean(base: Rep[Object], offset: Rep[Long]): Rep[Boolean] = 
    reflect("unsafe.getBoolean("+base+","+offset+")")
  def getBooleanVolatile(base: Rep[Object], offset: Rep[Long]): Rep[Boolean] = 
    reflect("unsafe.getBooleanVolatile("+base+","+offset+")")

  def getByte(base: Rep[Object], offset: Rep[Long]): Rep[Byte] = 
    reflect("unsafe.getByte("+base+","+offset+")")
  def getByteVolatile(base: Rep[Object], offset: Rep[Long]): Rep[Byte] = 
    reflect("unsafe.getByteVolatile("+base+","+offset+")")

  def getChar(base: Rep[Object], offset: Rep[Long]): Rep[Char] = 
    reflect("unsafe.getChar("+base+","+offset+")")
  def getCharVolatile(base: Rep[Object], offset: Rep[Long]): Rep[Char] = 
    reflect("unsafe.getCharVolatile("+base+","+offset+")")

  def getShort(base: Rep[Object], offset: Rep[Long]): Rep[Short] = 
    reflect("unsafe.getShort("+base+","+offset+")")
  def getShortVolatile(base: Rep[Object], offset: Rep[Long]): Rep[Short] = 
    reflect("unsafe.getShortVolatile("+base+","+offset+")")

  def getInt(base: Rep[Object], offset: Rep[Long]): Rep[Int] = 
    reflect("unsafe.getInt("+base+","+offset+")")
  def getIntVolatile(base: Rep[Object], offset: Rep[Long]): Rep[Int] = 
    reflect("unsafe.getIntVolatile("+base+","+offset+")")

  def getLong(base: Rep[Object], offset: Rep[Long]): Rep[Long] = 
    reflect("unsafe.getLong("+base+","+offset+")")
  def getLongVolatile(base: Rep[Object], offset: Rep[Long]): Rep[Long] = 
    reflect("unsafe.getLongVolatile("+base+","+offset+")")

  def getFloat(base: Rep[Object], offset: Rep[Long]): Rep[Float] = 
    reflect("unsafe.getFloat("+base+","+offset+")")
  def getFloatVolatile(base: Rep[Object], offset: Rep[Long]): Rep[Float] = 
    reflect("unsafe.getFloatVolatile("+base+","+offset+")")

  def getDouble(base: Rep[Object], offset: Rep[Long]): Rep[Double] = 
    reflect("unsafe.getDouble("+base+","+offset+")")
  def getDoubleVolatile(base: Rep[Object], offset: Rep[Long]): Rep[Double] = 
    reflect("unsafe.getDoubleVolatile("+base+","+offset+")")


  def putObject(base: Rep[Object], offset: Rep[Long], value: Rep[Object]): Rep[Unit] = 
    reflect("unsafe.putObject("+base+","+offset+", "+value+")")
  def putObjectVolatile(base: Rep[Object], offset: Rep[Long], value: Rep[Object]): Rep[Unit] = 
    reflect("unsafe.putObjectVolatile("+base+","+offset+", "+value+")")

  def putBoolean(base: Rep[Object], offset: Rep[Long], value: Rep[Boolean]): Rep[Unit] = 
    reflect("unsafe.putBoolean("+base+","+offset+", "+value+")")
  def putBooleanVolatile(base: Rep[Object], offset: Rep[Long], value: Rep[Boolean]): Rep[Unit] = 
    reflect("unsafe.putBooleanVolatile("+base+","+offset+", "+value+")")

  def putByte(base: Rep[Object], offset: Rep[Long], value: Rep[Byte]): Rep[Unit] = 
    reflect("unsafe.putByte("+base+","+offset+", "+value+")")
  def putByteVolatile(base: Rep[Object], offset: Rep[Long], value: Rep[Byte]): Rep[Unit] = 
    reflect("unsafe.putByteVolatile("+base+","+offset+", "+value+")")

  def putChar(base: Rep[Object], offset: Rep[Long], value: Rep[Char]): Rep[Unit] = 
    reflect("unsafe.putChar("+base+","+offset+", "+value+")")
  def putCharVolatile(base: Rep[Object], offset: Rep[Long], value: Rep[Char]): Rep[Unit] = 
    reflect("unsafe.putCharVolatile("+base+","+offset+", "+value+")")

  def putShort(base: Rep[Object], offset: Rep[Long], value: Rep[Short]): Rep[Unit] = 
    reflect("unsafe.putShort("+base+","+offset+", "+value+")")
  def putShortVolatile(base: Rep[Object], offset: Rep[Long], value: Rep[Short]): Rep[Unit] = 
    reflect("unsafe.putShortVolatile("+base+","+offset+", "+value+")")

  def putInt(base: Rep[Object], offset: Rep[Long], value: Rep[Int]): Rep[Unit] = 
    reflect("unsafe.putInt("+base+","+offset+", "+value+")")
  def putIntVolatile(base: Rep[Object], offset: Rep[Long], value: Rep[Int]): Rep[Unit] = 
    reflect("unsafe.putIntVolatile("+base+","+offset+", "+value+")")

  def putLong(base: Rep[Object], offset: Rep[Long], value: Rep[Long]): Rep[Unit] = 
    reflect("unsafe.putLong("+base+","+offset+", "+value+")")
  def putLongVolatile(base: Rep[Object], offset: Rep[Long], value: Rep[Long]): Rep[Unit] = 
    reflect("unsafe.putLongVolatile("+base+","+offset+", "+value+")")

  def putFloat(base: Rep[Object], offset: Rep[Long], value: Rep[Float]): Rep[Unit] = 
    reflect("unsafe.putFloat("+base+","+offset+", "+value+")")
  def putFloatVolatile(base: Rep[Object], offset: Rep[Long], value: Rep[Float]): Rep[Unit] = 
    reflect("unsafe.putFloatVolatile("+base+","+offset+", "+value+")")

  def putDouble(base: Rep[Object], offset: Rep[Long], value: Rep[Double]): Rep[Unit] = 
    reflect("unsafe.putDouble("+base+","+offset+", "+value+")")
  def putDoubleVolatile(base: Rep[Object], offset: Rep[Long], value: Rep[Double]): Rep[Unit] = 
    reflect("unsafe.putDoubleVolatile("+base+","+offset+", "+value+")")




  def allocateInstance(clazz: Class[_]): Rep[Object] = 
    reflect("unsafe.allocateInstance("+clazz+")")

}





class Runtime_Str(metaProvider: MetaAccessProvider) extends Runtime {

    def invoke(method: ResolvedJavaMethod, args: Array[Rep[Object]]): Rep[Object] =
        reflect(""+method+".invoke("+args.mkString(",")+")")

    def typeIsInstance(typ: ResolvedJavaType, value: Rep[Object]): Rep[Boolean] = {
        reflect(""+value+".isInstanceOf["+typ.toJava+"]")
    }

    def monitorEnter(value: Rep[Object]): Unit = {
        nullCheck(value)
        unsafe.monitorEnter(value)
    }

    def monitorExit(value: Rep[Object]): Unit = {
        nullCheck(value)
        unsafe.monitorEnter(value)
    }

    def newObject(typ: ResolvedJavaType): Rep[Object] = { //} throws InstantiationException {
        unsafe.allocateInstance(typ.toJava());
    }

    def newArray(typ: ResolvedJavaType, size: Rep[Int]): Rep[Object] = { // throws InstantiationException {
        reflect("new Array["+typ.toJava()+"]("+size+")");
    }

    def newArray(typ: Class[_], size: Rep[Int]): Rep[Object] = { // throws InstantiationException {
        reflect("new Array["+typ+"]("+size+")");
    }

    def newMultiArray(typ: ResolvedJavaType, dimensions: Array[Rep[Int]]): Rep[Object] = { // throws InstantiationException {
        reflect("new Array["+typ.toJava()+"]("+dimensions.mkString(",")+")");
    }

    def getFieldObject(base: Rep[Object], field: ResolvedJavaField): Rep[AnyRef] = {
        val offset = resolveOffset(field);
        if (isVolatile(field)) {
            unsafe.getObjectVolatile(resolveBase(base, field), offset)
        } else {
            unsafe.getObject(resolveBase(base, field), offset)
        }
    }

    def getFieldBoolean(base: Rep[Object], field: ResolvedJavaField): Rep[Boolean] = {
        val offset = resolveOffset(field);
        if (isVolatile(field)) {
            unsafe.getBooleanVolatile(resolveBase(base, field), offset)
        } else {
            unsafe.getBoolean(resolveBase(base, field), offset)
        }
    }

    def getFieldByte(base: Rep[Object], field: ResolvedJavaField): Rep[Byte] = {
        val offset = resolveOffset(field);
        if (isVolatile(field)) {
            unsafe.getByteVolatile(resolveBase(base, field), offset)
        } else {
            unsafe.getByte(resolveBase(base, field), offset)
        }
    }

    def getFieldChar(base: Rep[Object], field: ResolvedJavaField): Rep[Char] = {
        val offset = resolveOffset(field);
        if (isVolatile(field)) {
            unsafe.getCharVolatile(resolveBase(base, field), offset)
        } else {
            unsafe.getChar(resolveBase(base, field), offset)
        }
    }

    def getFieldShort(base: Rep[Object], field: ResolvedJavaField): Rep[Short] = {
        val offset = resolveOffset(field);
        if (isVolatile(field)) {
            unsafe.getShortVolatile(resolveBase(base, field), offset)
        } else {
            unsafe.getShort(resolveBase(base, field), offset)
        }
    }

    def getFieldInt(base: Rep[Object], field: ResolvedJavaField): Rep[Int] = {
        val offset = resolveOffset(field);
        if (isVolatile(field)) {
            unsafe.getIntVolatile(resolveBase(base, field), offset)
        } else {
            unsafe.getInt(resolveBase(base, field), offset)
        }
    }

    def getFieldLong(base: Rep[Object], field: ResolvedJavaField): Rep[Long] = {
        val offset = resolveOffset(field);
        if (isVolatile(field)) {
            unsafe.getLongVolatile(resolveBase(base, field), offset)
        } else {
            unsafe.getLong(resolveBase(base, field), offset)
        }
    }

    def getFieldDouble(base: Rep[Object], field: ResolvedJavaField): Rep[Double] = {
        val offset = resolveOffset(field);
        if (isVolatile(field)) {
            unsafe.getDoubleVolatile(resolveBase(base, field), offset)
        } else {
            unsafe.getDouble(resolveBase(base, field), offset)
        }
    }

    def getFieldFloat(base: Rep[Object], field: ResolvedJavaField): Rep[Float] = {
        val offset = resolveOffset(field);
        if (isVolatile(field)) {
            unsafe.getFloatVolatile(resolveBase(base, field), offset)
        } else {
            unsafe.getFloat(resolveBase(base, field), offset)
        }
    }

    def setFieldObject(value: Rep[Object], base: Rep[Object], field: ResolvedJavaField): Unit = {
        val offset = resolveOffset(field);
        if (isVolatile(field)) {
            unsafe.putObjectVolatile(resolveBase(base, field), offset, value)
        } else {
            unsafe.putObject(resolveBase(base, field), offset, value)
        }
    }

    def setFieldInt(value: Rep[Int], base: Rep[Object], field: ResolvedJavaField): Unit = {
        val offset = resolveOffset(field);
        if (isVolatile(field)) {
            unsafe.putIntVolatile(resolveBase(base, field), offset, value)
        } else {
            unsafe.putInt(resolveBase(base, field), offset, value)
        }
    }


    def setFieldFloat(value: Rep[Float], base: Rep[Object], field: ResolvedJavaField): Unit = {
        val offset = resolveOffset(field);
        if (isVolatile(field)) {
            unsafe.putFloatVolatile(resolveBase(base, field), offset, value)
        } else {
            unsafe.putFloat(resolveBase(base, field), offset, value)
        }
    }

    def setFieldDouble(value: Rep[Double], base: Rep[Object], field: ResolvedJavaField): Unit = {
        val offset = resolveOffset(field);
        if (isVolatile(field)) {
            unsafe.putDoubleVolatile(resolveBase(base, field), offset, value);
        } else {
            unsafe.putDouble(resolveBase(base, field), offset, value);
        }
    }

    def setFieldLong(value: Rep[Long], base: Rep[Object], field: ResolvedJavaField): Unit = {
        val offset = resolveOffset(field);
        if (isVolatile(field)) {
            unsafe.putDoubleVolatile(resolveBase(base, field), offset, value);
        } else {
            unsafe.putDouble(resolveBase(base, field), offset, value);
        }
    }

    def getArrayByte(index: Rep[Long], array: Rep[Object]): Rep[Byte] = {
        checkArray(array, index);
        return unsafe.getByte(array, (Unsafe.ARRAY_BYTE_BASE_OFFSET) + Unsafe.ARRAY_BYTE_INDEX_SCALE.toLong * index);
    }

    def getArrayChar(index: Rep[Long], array: Rep[Object]): Rep[Char] = {
        checkArray(array, index);
        return unsafe.getChar(array, Unsafe.ARRAY_CHAR_BASE_OFFSET + Unsafe.ARRAY_CHAR_INDEX_SCALE * index);
    }

    def getArrayShort(index: Rep[Long], array: Rep[Object]): Rep[Short] = {
        checkArray(array, index);
        return unsafe.getShort(array, Unsafe.ARRAY_SHORT_BASE_OFFSET + Unsafe.ARRAY_SHORT_INDEX_SCALE * index);
    }

    def getArrayInt(index: Rep[Long], array: Rep[Object]): Rep[Int] = {
        checkArray(array, index);
        return unsafe.getInt(array, Unsafe.ARRAY_INT_BASE_OFFSET + Unsafe.ARRAY_INT_INDEX_SCALE * index);
    }

    def getArrayLong(index: Rep[Long], array: Rep[Object]): Rep[Long] = {
        checkArray(array, index);
        return unsafe.getLong(array, Unsafe.ARRAY_LONG_BASE_OFFSET + Unsafe.ARRAY_LONG_INDEX_SCALE * index);
    }

    def getArrayDouble(index: Rep[Long], array: Rep[Object]): Rep[Double] = {
        checkArray(array, index);
        return unsafe.getDouble(array, Unsafe.ARRAY_DOUBLE_BASE_OFFSET + Unsafe.ARRAY_DOUBLE_INDEX_SCALE * index);
    }

    def getArrayFloat(index: Rep[Long], array: Rep[Object]): Rep[Float] = {
        checkArray(array, index);
        return unsafe.getFloat(array, Unsafe.ARRAY_FLOAT_BASE_OFFSET + Unsafe.ARRAY_FLOAT_INDEX_SCALE * index);
    }

    def getArrayObject(index: Rep[Long], array: Rep[Object]): Rep[Object] = {
        checkArray(array, index);
        return unsafe.getObject(array, Unsafe.ARRAY_OBJECT_BASE_OFFSET + Unsafe.ARRAY_OBJECT_INDEX_SCALE * index);
    }

    def setArrayByte(value: Rep[Byte], index: Rep[Long], array: Rep[Object]): Unit = {
        checkArray(array, index);
        if (array.isInstanceOf[Array[Boolean]]) {
            checkArrayType(array, classOf[Boolean]);
        } else {
            checkArrayType(array, classOf[Byte]);
        }
        unsafe.putByte(array, Unsafe.ARRAY_BYTE_BASE_OFFSET + Unsafe.ARRAY_BYTE_INDEX_SCALE * index, value);
    }

    def setArrayChar(value: Rep[Char], index: Rep[Long], array: Rep[Object]): Unit = {
        checkArray(array, index);
        checkArrayType(array, classOf[Char]);
        unsafe.putChar(array, Unsafe.ARRAY_CHAR_BASE_OFFSET + Unsafe.ARRAY_CHAR_INDEX_SCALE * index, value);
    }

    def setArrayShort(value: Rep[Short], index: Rep[Long], array: Rep[Object]): Unit = {
        checkArray(array, index);
        checkArrayType(array, classOf[Short]);
        unsafe.putShort(array, Unsafe.ARRAY_SHORT_BASE_OFFSET + Unsafe.ARRAY_SHORT_INDEX_SCALE * index, value);
    }

    def setArrayInt(value: Rep[Int], index: Rep[Long], array: Rep[Object]): Unit = {
        checkArray(array, index);
        checkArrayType(array, classOf[Int]);
        unsafe.putInt(array, Unsafe.ARRAY_INT_BASE_OFFSET + Unsafe.ARRAY_INT_INDEX_SCALE * index, value);
    }

    def setArrayLong(value: Rep[Long], index: Rep[Long], array: Rep[Object]): Unit = {
        checkArray(array, index);
        checkArrayType(array, classOf[Long]);
        unsafe.putLong(array, Unsafe.ARRAY_LONG_BASE_OFFSET + Unsafe.ARRAY_LONG_INDEX_SCALE * index, value);
    }

    def setArrayFloat(value: Rep[Float], index: Rep[Long], array: Rep[Object]): Unit = {
        checkArray(array, index);
        checkArrayType(array, classOf[Float]);
        unsafe.putFloat(array, Unsafe.ARRAY_FLOAT_BASE_OFFSET + Unsafe.ARRAY_FLOAT_INDEX_SCALE * index, value);
    }

    def setArrayDouble(value: Rep[Double], index: Rep[Long], array: Rep[Object]): Unit = {
        checkArray(array, index);
        checkArrayType(array, classOf[Double]);
        unsafe.putDouble(array, Unsafe.ARRAY_DOUBLE_BASE_OFFSET + Unsafe.ARRAY_DOUBLE_INDEX_SCALE * index, value);
    }

    def setArrayObject(value: Rep[Object], index: Rep[Long], array: Rep[Object]): Unit = {
        checkArray(array, index);
        checkArrayType(array, if (value != null) value.getClass() else null);
        unsafe.putObject(array, Unsafe.ARRAY_OBJECT_BASE_OFFSET + Unsafe.ARRAY_OBJECT_INDEX_SCALE * index, value);
    }

    def nullCheck(value: Rep[Object]): Rep[Object] = 
      reflect("if ("+value+" == null) throw new NullPointerException() else "+value)

    def checkArrayType(array: Rep[Object], arrayType: Class[_]): Unit = reflect("""{
        if (arrayType == null) {
            return;
        }
        val typ: ResolvedJavaType = metaProvider.getResolvedJavaType(array.getClass()).componentType();
        if (!typ.toJava().isAssignableFrom(arrayType)) {
            throw new ArrayStoreException(arrayType.getName());
        }
    }""")

    def checkArray(array: Rep[Object], index: Rep[Long]): Unit = reflect("""{
        nullCheck(array);
        val typ: ResolvedJavaType = metaProvider.getResolvedJavaType(array.getClass());
        if (!typ.isArrayClass()) {
            throw new ArrayStoreException(array.getClass().getName());
        }
        if (index < 0 || index >= arrayLength(array)) {
            throw new ArrayIndexOutOfBoundsException(index.toInt);
        }
    }""")

    def arrayLength(array: Rep[Object]): Rep[Int] = {
        assert(array != null);
        return java.lang.reflect.Array.getLength(array);
    }

    def isVolatile(field: ResolvedJavaField): Boolean = {
        return Modifier.isVolatile(field.accessFlags());
    }

    def resolveOffset(field: ResolvedJavaField): Long = {
        return field.asInstanceOf[HotSpotResolvedJavaField].offset();
    }

    def resolveBase(base: Rep[Object], field: ResolvedJavaField): Rep[Object] = 
      reflect("if ("+base+" == null) "+field.holder().toJava()+" else "+base)

}



object Frame {
    final val EMPTY_ARRAY = new Array[Object](0)
    final val PARENT_FRAME_SLOT = 0;
    final val MIN_FRAME_SIZE = 1;
}


// local array is staged

class Frame_Str0(val numLocals: Int, parent: Frame) extends Frame {
    import Frame._
    assert(numLocals >= MIN_FRAME_SIZE);

    var locals: Rep[Array[Object]] = _//reflect("new Array[Object]("+numLocals+")")
    var primitiveLocals: Rep[Array[Long]] = _//reflect("new Array[Long]("+numLocals+")")

    //reflect("locals(PARENT_FRAME_SLOT) = parent")

    def this(numLocals: Int) = this(numLocals, null);

    def getObject(index: Int): Rep[Object] = {
        reflect(locals+"("+index+")")
    }

    def setObject(index: Int, value: Rep[Object]): Unit = {
        reflect(locals+"("+index+") = "+value)
    }

    def getFloat(index: Int): Rep[Float] = {
        return unsafe.getFloat(primitiveLocals, index.toLong * Unsafe.ARRAY_LONG_INDEX_SCALE + Unsafe.ARRAY_LONG_BASE_OFFSET);
    }

    def setFloat(index: Int, value: Rep[Float]): Unit = {
        unsafe.putFloat(primitiveLocals, index.toLong * Unsafe.ARRAY_LONG_INDEX_SCALE + Unsafe.ARRAY_LONG_BASE_OFFSET, value);
    }

    def getLong(index: Int): Rep[Long] = {
        return unsafe.getLong(primitiveLocals, index.toLong * Unsafe.ARRAY_LONG_INDEX_SCALE + Unsafe.ARRAY_LONG_BASE_OFFSET);
    }

    def setLong(index: Int, value: Rep[Long]): Unit = {
        unsafe.putLong(primitiveLocals, index.toLong * Unsafe.ARRAY_LONG_INDEX_SCALE + Unsafe.ARRAY_LONG_BASE_OFFSET, value);
    }

    def getInt(index: Int): Rep[Int] = {
        return unsafe.getInt(primitiveLocals, index.toLong * Unsafe.ARRAY_LONG_INDEX_SCALE + Unsafe.ARRAY_LONG_BASE_OFFSET);
    }

    def setInt(index: Int, value: Rep[Int]): Unit = {
        unsafe.putInt(primitiveLocals, index.toLong * Unsafe.ARRAY_LONG_INDEX_SCALE + Unsafe.ARRAY_LONG_BASE_OFFSET, value);
    }

    def getDouble(index: Int): Rep[Double] = {
        return unsafe.getDouble(primitiveLocals, index.toLong * Unsafe.ARRAY_LONG_INDEX_SCALE + Unsafe.ARRAY_LONG_BASE_OFFSET);
    }

    def setDouble(index: Int, value: Rep[Double]): Unit = {
        unsafe.putDouble(primitiveLocals, index.toLong * Unsafe.ARRAY_LONG_INDEX_SCALE + Unsafe.ARRAY_LONG_BASE_OFFSET, value);
    }

    def getParentFrame(level: Int): Frame = {
        assert(level >= 0);
        if (level == 0) {
            return this;
        } else {
            return getObject(PARENT_FRAME_SLOT).asInstanceOf[Frame].getParentFrame(level - 1);
        }
    }

    def getTopFrame(): Frame = {
        val parentFrame = getObject(PARENT_FRAME_SLOT).asInstanceOf[Frame];
        if (parentFrame == null) {
            return this;
        } else {
            return parentFrame.getTopFrame();
        }
    }

    def getArguments(argOffset: Int): Rep[Array[Object]] = {
        return getObject(argOffset).asInstanceOf[Rep[Array[Object]]]; // TODO: dynamic cast
    }

    def size: Int = {
        return numLocals//locals.length;
    }
    
}


// local array contains staged values

class Frame_Str(val numLocals: Int, val parent: Frame) extends Frame {
    import Frame._
    assert(numLocals >= MIN_FRAME_SIZE);

    val locals: Array[Rep[Object]] = new Array[Rep[Object]](numLocals)

    //reflect("locals(PARENT_FRAME_SLOT) = parent")

    def this(numLocals: Int) = this(numLocals, null);

    def getObject(index: Int): Rep[Object] = {
        locals(index)
    }

    def setObject(index: Int, value: Rep[Object]): Unit = {
        locals(index) = value
    }

    def getFloat(index: Int): Rep[Float] = {
        locals(index).asInstanceOf[Rep[Float]]
    }

    def setFloat(index: Int, value: Rep[Float]): Unit = {
        locals(index) = value.asInstanceOf[Rep[Object]]
    }

    def getLong(index: Int): Rep[Long] = {
        locals(index).asInstanceOf[Rep[Long]]
    }

    def setLong(index: Int, value: Rep[Long]): Unit = {
        locals(index) = value.asInstanceOf[Rep[Object]]
    }

    def getInt(index: Int): Rep[Int] = {
        locals(index).asInstanceOf[Rep[Int]]
    }

    def setInt(index: Int, value: Rep[Int]): Unit = {
        locals(index) = value.asInstanceOf[Rep[Object]]
    }

    def getDouble(index: Int): Rep[Double] = {
        locals(index).asInstanceOf[Rep[Double]]
    }

    def setDouble(index: Int, value: Rep[Double]): Unit = {
        locals(index) = value.asInstanceOf[Rep[Object]]
    }

    def getParentFrame(level: Int): Frame = {
        assert(level >= 0);
        if (level == 0) {
            return this;
        } else {
            return parent.getParentFrame(level - 1);
        }
    }

    def getTopFrame(): Frame = {
        val parentFrame = parent
        if (parentFrame == null) {
            return this;
        } else {
            return parentFrame.getTopFrame();
        }
    }

    def getArguments(argOffset: Int): Rep[Array[Object]] = {
        return getObject(argOffset).asInstanceOf[Rep[Array[Object]]]; // TODO: dynamic cast
    }

    def size: Int = {
        return numLocals//locals.length;
    }
    
}




object InterpreterFrame {
    final val BASE_LENGTH = 3;

    final val METHOD_FRAME_SLOT = 1;
    final val BCI_FRAME_SLOT = 2;

    final val DOUBLE = 2;
    final val SINGLE = 1;
}


class InterpreterFrame_Str(var method: ResolvedJavaMethod, parent: InterpreterFrame, additionalStackSpace: Int) 
extends Frame_Str(method.maxLocals() + method.maxStackSize() + InterpreterFrame.BASE_LENGTH + additionalStackSpace, parent) 
with InterpreterFrame {

    import Frame._
    import InterpreterFrame._

    var bci: Int = _

    var returnValue: Rep[Object] = _

    assert(additionalStackSpace >= 0);

    setMethod(method);
    setBCI(0);
        

    /** Pointer to the top-most stack frame element. */
    private var tos: Int = BASE_LENGTH;

    def this(method: ResolvedJavaMethod, additionalStackSpace: Int) {
        this(method, null, additionalStackSpace);
    }

    def create(method: ResolvedJavaMethod, hasReceiver: Boolean, additionalStackSpace: Int, useParentArguments: Boolean): InterpreterFrame = {
        val frame = new InterpreterFrame_Str(method, this, additionalStackSpace);

        if (useParentArguments) {
            val length = method.signature().argumentSlots(hasReceiver);
            assert(length >= 0);

            frame.pushVoid(method.maxLocals());
            if (length > 0) {
                copyArguments(frame, length);
                popVoid(length);
            }
        }

        return frame;
    }

    def copy() = {
      val frame = new InterpreterFrame_Str(method, parent, additionalStackSpace);
      System.arraycopy(locals, 0, frame.locals, 0, locals.length)
      //frame.locals = locals
      //frame.primitiveLocals = primitiveLocals
      frame.returnValue = returnValue
      frame.bci = bci
      frame.tos = tos
      frame
    }


    def resolveLocalIndex(index: Int): Int = {
        assert(index >= 0);
        return BASE_LENGTH + index;
    }

    def depth(): Int = {
        var depth = 1;
        var frame: InterpreterFrame = this;
        while ({ frame = frame.getParentFrame(); frame != null}) {
            depth+=1;
        }
        return depth;
    }

    def stackTos(): Int = {
        return BASE_LENGTH + getMethod().maxLocals();
    }

    private def copyArguments(dest: InterpreterFrame_Str, length: Int): Unit = {
        System.arraycopy(locals, tosSingle(length - 1), dest.locals, BASE_LENGTH, length)
    }


    def getReturnValue(): Rep[Object] = {
      returnValue
    }
    def setReturnValueObject(value: Rep[Object]): Unit = {
      returnValue = value
    }
    def setReturnValueInt(value: Rep[Int]): Unit = {
      returnValue = value.asInstanceOf[Rep[Object]]
    }
    def setReturnValueLong(value: Rep[Long]): Unit = {
      returnValue = value.asInstanceOf[Rep[Object]]
    }
    def setReturnValueFloat(value: Rep[Float]): Unit = {
      returnValue = value.asInstanceOf[Rep[Object]]
    }
    def setReturnValueDouble(value: Rep[Double]): Unit = {
      returnValue = value.asInstanceOf[Rep[Object]]
    }

    def peekReceiver(method: ResolvedJavaMethod): Rep[Object] = {
        return getObject(tosSingle(method.signature().argumentSlots(false)));
    }

    def pushBoth(oValue: Rep[Object], intValue: Rep[Int]): Unit = {
        incrementTos(SINGLE);
        setObject(tosSingle(0), oValue);
        setInt(tosSingle(0), intValue);
    }

    class OVHack
    implicit val ovhack = new OVHack

    def pushBoth(oValue: Rep[Object], longValue: Rep[Long])(implicit e: OVHack): Unit = {
        incrementTos(SINGLE);
        setObject(tosSingle(0), oValue);
        setLong(tosSingle(0), longValue);
    }

    def pushObject(value: Rep[Object]): Unit = {
        incrementTos(SINGLE);
        setObject(tosSingle(0), value);
    }

    def pushBoolean(value: Rep[Boolean]): Unit = {
        pushInt(if_ (value) (1) (0));
    }

    def pushByte(value: Rep[Byte]): Unit = {
        pushInt(value);
    }

    def pushShort(value: Rep[Short]): Unit = {
        pushInt(value);
    }

    def pushChar(value: Rep[Char]): Unit = {
        pushInt(value);
    }

    def pushInt(value: Rep[Int]): Unit = {
        incrementTos(SINGLE);
        setInt(tosSingle(0), value);
    }

    def pushDouble(value: Rep[Double]): Unit = {
        incrementTos(DOUBLE);
        setDouble(tosDouble(0), value);
    }

    def pushFloat(value: Rep[Float]): Unit = {
        incrementTos(SINGLE);
        setFloat(tosSingle(0), value);
    }

    def pushLong(value: Rep[Long]): Unit = {
        incrementTos(DOUBLE);
        setLong(tosDouble(0), value);
    }

    def popBoolean(): Rep[Boolean] = {
        val value = popInt();
        assert(value == 0 || value == 1);
        return value == 1;
    }

    def popByte(): Rep[Byte] = {
        val value = popInt();
        //assert (value >= Byte.MinValue && value <= Byte.MaxValue);
        return value.toByte;
    }

    def popChar(): Rep[Char] = {
        val value = popInt();
        //assert (value >= Char.MinValue && value <= Char.MaxValue);
        return value.toChar;
    }

    def popShort(): Rep[Short] = {
        val value = popInt();
        //assert (value >= Short.MinValue && value <= Short.MaxValue);
        return value.toShort;
    }

    def popInt(): Rep[Int] = {
        val value = getInt(tosSingle(0));
        decrementTos(SINGLE);
        return value;
    }

    def popDouble(): Rep[Double] = {
        val value = getDouble(tosDouble(0));
        decrementTos(DOUBLE);
        return value;
    }

    def popFloat(): Rep[Float] = {
        val value = getFloat(tosSingle(0));
        decrementTos(SINGLE);
        return value;
    }

    def popLong(): Rep[Long] = {
        val value = getLong(tosDouble(0));
        decrementTos(DOUBLE);
        return value;
    }

    def popObject(): Rep[Object] = {
        val value = getObject(tosSingle(0));
        decrementTos(SINGLE);
        return value;
    }

    def swapSingle(): Unit = {
        val tmpInt = getInt(tosSingle(1));
        val tmpObject = getObject(tosSingle(1));

        setInt(tosSingle(1), getInt(tosSingle(0)));
        setObject(tosSingle(1), getObject(tosSingle(0)));

        setInt(tosSingle(0), tmpInt);
        setObject(tosSingle(0), tmpObject);
    }

    def dupx1(): Unit = {
        val tosLong = getLong(tosSingle(0));
        val tosObject = getObject(tosSingle(0));

        swapSingle();

        pushBoth(tosObject, tosLong);
    }

    def dup2x1(): Unit = {
        val tosLong2 = getLong(tosSingle(2));
        val tosObject2 = getObject(tosSingle(2));
        val tosLong1 = getLong(tosSingle(1));
        val tosObject1 = getObject(tosSingle(1));
        val tosLong0 = getLong(tosSingle(0));
        val tosObject0 = getObject(tosSingle(0));

        popVoid(3);

        pushBoth(tosObject1, tosLong1);
        pushBoth(tosObject0, tosLong0);

        pushBoth(tosObject2, tosLong2);

        pushBoth(tosObject1, tosLong1);
        pushBoth(tosObject0, tosLong0);
    }

    def dup2x2(): Unit = {
        val tosLong3 = getLong(tosSingle(3));
        val tosObject3 = getObject(tosSingle(3));
        val tosLong2 = getLong(tosSingle(2));
        val tosObject2 = getObject(tosSingle(2));
        val tosLong1 = getLong(tosSingle(1));
        val tosObject1 = getObject(tosSingle(1));
        val tosLong0 = getLong(tosSingle(0));
        val tosObject0 = getObject(tosSingle(0));

        popVoid(4);

        pushBoth(tosObject1, tosLong1);
        pushBoth(tosObject0, tosLong0);

        pushBoth(tosObject3, tosLong3);
        pushBoth(tosObject2, tosLong2);

        pushBoth(tosObject1, tosLong1);
        pushBoth(tosObject0, tosLong0);
    }

    def dupx2(): Unit = {
        val tosLong2 = getLong(tosSingle(2));
        val tosObject2 = getObject(tosSingle(2));
        val tosLong1 = getLong(tosSingle(1));
        val tosObject1 = getObject(tosSingle(1));
        val tosLong0 = getLong(tosSingle(0));
        val tosObject0 = getObject(tosSingle(0));

        popVoid(3);

        pushBoth(tosObject0, tosLong0);
        pushBoth(tosObject2, tosLong2);
        pushBoth(tosObject1, tosLong1);
        pushBoth(tosObject0, tosLong0);
    }

    def dup(length: Int): Unit = {
        assert (length > 0);
        var i = 0
        while (i < length) {
            val valueN1 = getLong(tosSingle(length - 1));
            val valueO1 = getObject(tosSingle(length - 1));

            pushVoid(1);

            setLong(tosSingle(0), valueN1);
            setObject(tosSingle(0), valueO1);
            i += 1
        }
    }

    private def incrementTos(size: Int): Unit = {
        assert (size >= 0);
        tos += size;
    }

    private def decrementTos(size: Int): Unit = {
        assert (size >= 0);
        assert (tos - size >= stackTos());
        tos -= size;
    }

    private def tosDouble(offset: Int): Int = {
        assert (offset >= 0);
        return tos - DOUBLE - (offset * DOUBLE);
    }

    def tosSingle(offset: Int): Int = {
        assert (offset >= 0);
        return tos - SINGLE - offset;
    }

    def getStackTop(): Int = {
        return tos;
    }

    def pushVoid(count: Int): Unit = {
        incrementTos(count * SINGLE);
    }

    def popVoid(count: Int): Unit = {
        decrementTos(count * SINGLE);
    }

    def getConstantPool(): ConstantPool = {
        return getMethod().getConstantPool();
    }

    def setMethod(method: ResolvedJavaMethod): Unit = {
        this.method = method
    }

    def getMethod(): ResolvedJavaMethod = {
        return method
    }

    def setBCI(bci: Int): Unit = {
        this.bci = bci
    }

    def getBCI(): Int = {
        return bci
    }

    /*def pushTo(childFrame: InterpreterFrame, argumentSlots: Int): Unit = {
        System.arraycopy(locals, tos - argumentSlots, childFrame.locals,
                        Frame.MIN_FRAME_SIZE, argumentSlots);

        System.arraycopy(primitiveLocals, tos - argumentSlots, childFrame.primitiveLocals,
                        Frame.MIN_FRAME_SIZE, argumentSlots);
        popVoid(argumentSlots);
    }*/

    def getParentFrame(): InterpreterFrame = {
        //return getObject(PARENT_FRAME_SLOT).asInstanceOf[InterpreterFrame];
        //throw new Exception("not supported")
        parent.asInstanceOf[InterpreterFrame]
    }

    def dispose(): Unit = {
        // Clear out references in locals array.
        Arrays.fill(locals.asInstanceOf[Array[Object]], null)
        returnValue = null
    }

/*    override def toString(): String = {
        val method = getMethod();
        val b = new StringBuilder(getMethod().toStackTraceElement(getBCI()).toString());
        for (i <- 0 until tos) {
            val obj = getObject(tosSingle(i));
            val primitive = getLong(tosSingle(i));

            var objectString: String = null;
            if (obj != null) {
                objectString = obj.getClass().getSimpleName() + "@" + Integer.toHexString(obj.hashCode());
            }
            val primitiveString = "0x" + java.lang.Long.toHexString(primitive).toUpperCase();
            var typeString: String = null;

            val index = tosSingle(i);
            if (index == METHOD_FRAME_SLOT) {
                typeString = "method";
            } else if (index == BCI_FRAME_SLOT) {
                typeString = "bci";
            } else if (index == PARENT_FRAME_SLOT) {
                typeString = "parent";
            } else if (index < BASE_LENGTH + method.maxLocals()) {
                typeString = "local " + (index - BASE_LENGTH);
            } else {
                typeString = "stack";
            }
            b.append(String.format("%n [%d] %7s Primitive: %10s Object: %s", index:Integer, typeString, primitiveString, objectString));
        }
        if (getParentFrame() != null) {
            b.append("\n").append(getParentFrame().toString());
        }
        return b.toString();
    }
*/
    def popStack(): Unit = {
        // TODO(chumer): prevent popping local variables.
        popVoid(tos - stackTos());
    }

}

}