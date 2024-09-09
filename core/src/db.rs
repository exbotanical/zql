pub struct Database {}

impl Database {
    pub fn new() -> Self {
        todo!()
    }

    fn open(&mut self) {
        // If does not exist, create
        // Open

        // If size == 0, init
        // Else get page size

        // Mmap data file
    }

    // Acquire lock on fd
    // If !db.read_only, only one process can hold the write lock
    fn lock(&mut self) {
        // Declare current time, t; timeout
        // Loop
        // // If time_since(t) > timeout
        // // Lock db_file
        // If unable to lock, sleep and loop again
    }
}
