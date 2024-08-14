package simpledb;

import java.io.Serializable;
import java.util.*;

/**
 * TupleDesc describes the schema of a tuple.
 */
public class TupleDesc implements Serializable {

    /**
     * A help class to facilitate organizing the information of each field
     * */
    public static class TDItem implements Serializable {

        private static final long serialVersionUID = 1L;

        /**
         * The type of the field
         * */
        public final Type fieldType;
        
        /**
         * The name of the field
         * */
        public final String fieldName;

        public TDItem(Type t, String n) {
            this.fieldName = n;
            this.fieldType = t;
        }

        public String toString() {
            return fieldName + "(" + fieldType + ")";
        }
    }

    private TDItem[] tdItemArray;

    /**
     * @return
     *        An iterator which iterates over all the field TDItems
     *        that are included in this TupleDesc
     * */
    public Iterator<TDItem> iterator() {
        // some code goes here
        return Arrays.asList(tdItemArray).iterator();
    }

    private static final long serialVersionUID = 1L;

    /**
     * Create a new TupleDesc with typeAr.length fields with fields of the
     * specified types, with associated named fields.
     * 
     * @param typeAr
     *            array specifying the number of and types of fields in this
     *            TupleDesc. It must contain at least one entry.
     * @param fieldAr
     *            array specifying the names of the fields. Note that names may
     *            be null.
     */
    public TupleDesc(Type[] typeAr, String[] fieldAr) {
        // some code goes here
        if (typeAr.length >= 1) {
            this.tdItemArray = new TDItem[typeAr.length];
            for (int k = 0; k < typeAr.length; k++) {
                TDItem tempItem = new TDItem(typeAr[k], fieldAr[k]);
                this.tdItemArray[k] = tempItem;
            }
        } else {
            throw new IndexOutOfBoundsException("Empty or invalid typeAr");
        }
    }

    /**
     * Constructor. Create a new tuple desc with typeAr.length fields with
     * fields of the specified types, with anonymous (unnamed) fields.
     * 
     * @param typeAr
     *            array specifying the number of and types of fields in this
     *            TupleDesc. It must contain at least one entry.
     */
    public TupleDesc(Type[] typeAr) {
        // some code goes here
        if (typeAr.length >= 1) {
            this.tdItemArray = new TDItem[typeAr.length];
            for (int k = 0; k < typeAr.length; k++) {
                TDItem tempItem = new TDItem(typeAr[k], null);
                this.tdItemArray[k] = tempItem;
            }
        } else {
            throw new IndexOutOfBoundsException("Empty or invalid typeAr");
        }
    }

    /**
     * @return the number of fields in this TupleDesc
     */
    public int numFields() {
        // some code goes here
        return this.tdItemArray.length;
    }

    /**
     * Gets the (possibly null) field name of the ith field of this TupleDesc.
     * 
     * @param i
     *            index of the field name to return. It must be a valid index.
     * @return the name of the ith field
     * @throws NoSuchElementException
     *             if i is not a valid field reference.
     */
    public String getFieldName(int i) throws NoSuchElementException {
        // some code goes here
        if (i < this.tdItemArray.length && i >= 0) {
            return this.tdItemArray[i].fieldName;
        } else {
            throw new NoSuchElementException("Index out of bounds for tdItemArray with length " + this.tdItemArray.length);
        }
    }

    /**
     * Gets the type of the ith field of this TupleDesc.
     * 
     * @param i
     *            The index of the field to get the type of. It must be a valid
     *            index.
     * @return the type of the ith field
     * @throws NoSuchElementException
     *             if i is not a valid field reference.
     */
    public Type getFieldType(int i) throws NoSuchElementException {
        // some code goes here
        if (i < this.tdItemArray.length && i >= 0) {
            return this.tdItemArray[i].fieldType;
        } else {
            throw new NoSuchElementException("Index out of bounds for tdItemArray with length " + this.tdItemArray.length);
        }
    }

    /**
     * Find the index of the field with a given name.
     * 
     * @param name
     *            name of the field.
     * @return the index of the field that is first to have the given name.
     * @throws NoSuchElementException
     *             if no field with a matching name is found.
     */
    public int fieldNameToIndex(String name) throws NoSuchElementException {
        // some code goes here
        if (name == null) {
            throw new NoSuchElementException("Cannot search null field name");
        }
        
        int index = 0;
        for (TDItem item : this.tdItemArray) {
            if (item.fieldName == null) {
                index++;
            } else if (!name.equals(item.fieldName)) {
                index++;
            } else {
                return index;
            }
        }
        throw new NoSuchElementException("No field with a matching name was found");
    }

    /**
     * @return The size (in bytes) of tuples corresponding to this TupleDesc.
     *         Note that tuples from a given TupleDesc are of a fixed size.
     */
    public int getSize() {
        // some code goes here
        int size = 0;
        for (TDItem item : this.tdItemArray) {
            size += item.fieldType.getLen();
        }
        return size;
    }

    /**
     * Merge two TupleDescs into one, with td1.numFields + td2.numFields fields,
     * with the first td1.numFields coming from td1 and the remaining from td2.
     * 
     * @param td1
     *            The TupleDesc with the first fields of the new TupleDesc
     * @param td2
     *            The TupleDesc with the last fields of the TupleDesc
     * @return the new TupleDesc
     */
    public static TupleDesc merge(TupleDesc td1, TupleDesc td2) {
        // some code goes here
        int totalNumFields = td1.numFields() + td2.numFields();

        Type[] typeAr = new Type[totalNumFields];
        String[] fieldAr = new String[totalNumFields];

        for (int k = 0; k < totalNumFields; k++) {
            if (k < td1.numFields()) {
                typeAr[k] = td1.getFieldType(k);
                fieldAr[k] = td1.getFieldName(k);
            } else {
                typeAr[k] = td2.getFieldType(k - td1.numFields());
                fieldAr[k] = td2.getFieldName(k - td1.numFields());
            }
        }

        TupleDesc tdMerged = new TupleDesc(typeAr, fieldAr);
        return tdMerged;
    }

    /**
     * Compares the specified object with this TupleDesc for equality. Two
     * TupleDescs are considered equal if they are the same size and if the n-th
     * type in this TupleDesc is equal to the n-th type in td.
     * 
     * @param o
     *            the Object to be compared for equality with this TupleDesc.
     * @return true if the object is equal to this TupleDesc.
     */
    public boolean equals(Object o) {
        // some code goes here
        if (o == null) {
            return false;
        } else if (this.getClass() != o.getClass()) {
            return false;
            // throw new UnsupportedOperationException("Expected input to be a TupleDesc object, but got something else");
        } else {
            TupleDesc oTupleDesc = (TupleDesc) o;
            if (this.numFields() != oTupleDesc.numFields()) {
                return false;
            } else {
                for (int k = 0; k < this.numFields(); k++) {
                    if (this.getFieldType(k) != oTupleDesc.getFieldType(k)) {
                        return false;
                    }
                }
                return true;
            }
        }
    }

    public int hashCode() {
        // If you want to use TupleDesc as keys for HashMap, implement this so
        // that equal objects have equals hashCode() results
        throw new UnsupportedOperationException("unimplemented");
    }

    /**
     * Returns a String describing this descriptor. It should be of the form
     * "fieldType[0](fieldName[0]), ..., fieldType[M](fieldName[M])", although
     * the exact format does not matter.
     * 
     * @return String describing this descriptor.
     */
    public String toString() {
        // some code goes here
        String s = "";
        for (int k = 0; k < this.tdItemArray.length; k++) {
            TDItem item = tdItemArray[k];
            s = item.fieldType + "[" + k + "](" + item.fieldName + "[" + k + "]),";
        }
        s = s.substring(0, s.length() - 1);
        return s;
    }
}
