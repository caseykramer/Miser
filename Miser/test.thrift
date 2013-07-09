# Test thrift IDL

// Single-line comment

/*
 * Block comment
 * which can have several lines
 */

namespace csharp ThriftTest 

struct UserProfile {
    1: i32 uid,
    2: string name,
    3: string blurb
}

service UserStorage {
    void store(1: UserProfile user),
    UserProfile retrieve(1: i32 uid)
}