package simpledb;

import java.io.*;
import java.util.*;

/**
 * HeapFile is an implementation of a DbFile that stores a collection of tuples
 * in no particular order. Tuples are stored on pages, each of which is a fixed
 * size, and the file is simply a collection of those pages. HeapFile works
 * closely with HeapPage. The format of HeapPages is described in the HeapPage
 * constructor.
 * 
 * @see simpledb.HeapPage#HeapPage
 * @author Sam Madden
 */
public class HeapFile implements DbFile {

    private File f;
    private TupleDesc td;

    /**
     * Constructs a heap file backed by the specified file.
     * 
     * @param f
     *            the file that stores the on-disk backing store for this heap
     *            file.
     */
    public HeapFile(File f, TupleDesc td) {
        // some code goes here
        this.f = f;
        this.td = td;
    }

    /**
     * Returns the File backing this HeapFile on disk.
     * 
     * @return the File backing this HeapFile on disk.
     */
    public File getFile() {
        // some code goes here
        return this.f;
    }

    /**
     * Returns an ID uniquely identifying this HeapFile. Implementation note:
     * you will need to generate this tableid somewhere ensure that each
     * HeapFile has a "unique id," and that you always return the same value for
     * a particular HeapFile. We suggest hashing the absolute file name of the
     * file underlying the heapfile, i.e. f.getAbsoluteFile().hashCode().
     * 
     * @return an ID uniquely identifying this HeapFile.
     */
    public int getId() {
        // some code goes here
        return this.f.getAbsoluteFile().hashCode();
    }

    /**
     * Returns the TupleDesc of the table stored in this DbFile.
     * 
     * @return TupleDesc of this DbFile.
     */
    public TupleDesc getTupleDesc() {
        // some code goes here
        return this.td;
    }

    // see DbFile.java for javadocs
    public Page readPage(PageId pid) {
        if (pid == null) {
            throw new IllegalArgumentException("Can't read page using a null value for page id");
        }
        
        try {
            RandomAccessFile randomAccessFile = new RandomAccessFile(this.f, "r");
            int pageSize = BufferPool.PAGE_SIZE;
            // System.out.println("pageSize = " + pageSize);
            byte[] bytesRead = new byte[pageSize];
            int offset = pid.pageNumber() * pageSize;
            randomAccessFile.seek(offset);
            randomAccessFile.read(bytesRead, 0, pageSize);
            randomAccessFile.close();
            return new HeapPage((HeapPageId) pid, bytesRead);
        } catch (IOException e) {
            throw new IllegalArgumentException("File not found or other error reading the file from disk");
        }
    }

    // see DbFile.java for javadocs
    public void writePage(Page page) throws IOException {
        // some code goes here
    	// not necessary for this assignment
    }

    /**
     * Returns the number of pages in this HeapFile.
     */
    public int numPages() {
        // some code goes here
        double pageSize = BufferPool.PAGE_SIZE;
        int fileSize = (int) this.f.length();
        int numPages = (int) Math.ceil(fileSize / pageSize);
        return numPages;
    }

    // see DbFile.java for javadocs
    public ArrayList<Page> insertTuple(TransactionId tid, Tuple t)
            throws DbException, IOException, TransactionAbortedException {
        // some code goes here
    	// not necessary for this assignment
        return null;
    }

    // see DbFile.java for javadocs
    public ArrayList<Page> deleteTuple(TransactionId tid, Tuple t) throws DbException,
            TransactionAbortedException {
        // some code goes here
    	// not necessary for this assignment
        return null;
    }

    // Returns a HeapFileIterator
    public DbFileIterator iterator(TransactionId tid) {
        // some code goes here
        return new HeapFileIterator(this, tid);
    }
}



/**
 * Helper class that implements the Java Iterator for tuples on
 * a HeapFile
 */
class HeapFileIterator implements DbFileIterator {
    Iterator<Tuple> it = null;
    int curpgno = 0;

    TransactionId tid;
    HeapFile hf;

    public HeapFileIterator(HeapFile hf, TransactionId tid) {
        this.hf = hf;
        this.tid = tid;
    }

    public void open()
        throws DbException, TransactionAbortedException {
        // Some Code Here
        HeapPageId pid = new HeapPageId(this.hf.getId(), curpgno);
        HeapPage curHeapPage = (HeapPage) Database.getBufferPool().getPage(this.tid, pid, Permissions.READ_ONLY);
        it = curHeapPage.iterator();
    }
    
    public boolean hasNext() throws DbException {
        // Some Code Here
        if (it == null) {
            return false;
        }
        try {
            while (curpgno < this.hf.numPages()) {
                if (it.hasNext()) {
                    return true;
                } else {
                    curpgno++;
                    if (curpgno < this.hf.numPages()) {
                        open();
                    }
                }
            }
            return false;
        } catch (DbException | TransactionAbortedException e) {
            throw new DbException("Error accessing page from BufferPool");
        }
    }

    // Return the next tuple in the HeapFile
    public Tuple next() throws TransactionAbortedException, DbException {
        // Some Code Here
        if (this.hasNext()) {
            return it.next();
        } else {
            throw new NoSuchElementException("End of tuples");
        }
    }

    public void rewind() throws DbException, TransactionAbortedException{
        // Not needed for this assignment
        close();
        open();
    }

    public void close() {
        // Some Code Here
        it = null;
        curpgno = 0;
    }
}