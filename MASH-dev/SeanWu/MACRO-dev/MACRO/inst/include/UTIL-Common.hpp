/*
 *      __  ______   __________  ____
 *     /  |/  /   | / ____/ __ \/ __ \
 *    / /|_/ / /| |/ /   / /_/ / / / /
 *   / /  / / ___ / /___/ _, _/ /_/ /
 *  /_/  /_/_/  |_\____/_/ |_|\____/
 *
 *  Common utility code
 *
 *  Sean Wu
 *  November 2018
 */

/* C++ */
#include <vector>
#include <algorithm>

/*
 from: https://stackoverflow.com/questions/838384/reorder-vector-using-a-vector-of-indices/1267878#1267878
 to reorder a vector 'value_iterator' based on order vector 'order_iterator'
*/
template< typename order_iterator, typename value_iterator >
void reorder( order_iterator order_begin, order_iterator order_end, value_iterator v )  {
    typedef typename std::iterator_traits< value_iterator >::value_type value_t;
    typedef typename std::iterator_traits< order_iterator >::value_type index_t;
    typedef typename std::iterator_traits< order_iterator >::difference_type diff_t;

    diff_t remaining = order_end - 1 - order_begin;
    for ( index_t s = index_t(), d; remaining > 0; ++ s ) {
        for ( d = order_begin[s]; d > s; d = order_begin[d] ) ;
        if ( d == s ) {
            -- remaining;
            value_t temp = v[s];
            while ( d = order_begin[d], d != s ) {
              std::swap( temp, v[d] );
                -- remaining;
            }
            v[s] = temp;
        }
    }
}

// template <typename T>
// void reorder( std::vector<T> & data, std::vector<std::size_t> const & order )
// {
//    for ( std::size_t i = 0; i < order.size(); ++i ) {
//       std::size_t original = order[i];
//       while ( i < original )  {
//          original = order[original];
//       }
//       std::swap( data[i], data[original] );
//    }
// }
//
// template <typename T>
// std::vector<size_t> ordered(std::vector<T> const& values) {
//     std::vector<size_t> indices(values.size());
//     std::iota(begin(indices), end(indices), static_cast<size_t>(0));
//
//     std::sort(
//         begin(indices), end(indices),
//         [&](size_t a, size_t b) { return values[a] >= values[b]; }
//     );
//     return indices;
// }
// 
// template <typename T>
// void reorder(std::vector<T>& data, std::vector<size_t>& order)
// {
//     assert(data.size() == order.size());
//
//     // for all elements to put in place
//     for( int i = 0; i < data.size() - 1; ++i )
//     {
//         // while the element i is not yet in place
//         while( i != order[i] )
//         {
//             // swap it with the element at its final place
//             int alt = order[i];
//             std::swap( data[i], data[alt] );
//             std::swap( order[i], order[alt] );
//         }
//     }
// }
