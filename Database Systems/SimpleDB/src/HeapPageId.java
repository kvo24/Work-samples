package simpledb;

/** Unique identifier for HeapPage objects. */
public class HeapPageId implements PageId {

	 int tableId;
	 int pgNo;
	 
    /**
     * Constructor. Create a page id structure for a specific page of a
     * specific table.
     *
     * @param tableId The table that is being referenced
     * @param pgNo The page number in that table.
     */
    public HeapPageId(int tableId, int pgNo) {
    	 this.tableId = tableId;
         this.pgNo = pgNo;
    }

    /** @return the table associated with this PageId */
    public int getTableId() {
        return tableId;
    }

    /**
     * @return the page number in the table tableid() associated with
     *   this PageId
     */
    public int pageNumber() {
        return pgNo;
    }

    /**
     * @return a hash code for this page, represented by the concatenation of
     *   the table number and the page number (needed if a PageId is used as a
     *   key in a hash table in the BufferPool, for example.)
     * @see BufferPool
     */
    public int hashCode() {
    	int code = (tableId << 16) + pgNo;
    	return code;
    }

    /**
     * Compares one PageId to another.
     *
     * @param o The object to compare against (must be a PageId)
     * @return true if the objects are equal (e.g., page numbers and table
     *   ids are the same)
     */
    public boolean equals(Object o) {
    	if (!(o instanceof HeapPageId))
    		return false;
         	HeapPageId p = (HeapPageId)o;
         	return tableId == p.tableId && pgNo == p.pgNo;
    }

    /**
     *  Return a representation of this object as an array of
     *  integers, for writing to disk.  Size of returned array must contain
     *  number of integers that corresponds to number of args to one of the
     *  constructors.
     */
    public int[] serialize() {
        // Not necessary for lab 1, 2, or 3
    	return null;
    }

}
