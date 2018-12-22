// /*
//  *      __  ______   __________  ____
//  *     /  |/  /   | / ____/ __ \/ __ \
//  *    / /|_/ / /| |/ /   / /_/ / / / /
//  *   / /  / / ___ / /___/ _, _/ /_/ /
//  *  /_/  /_/_/  |_\____/_/ |_|\____/
//  *
//  *  Common utility code
//  *
//  *  Sean Wu
//  *  November 2018
//  */
//
// /* C++ */
// #include <vector>
// #include <algorithm>
//
// /*
//  from: https://stackoverflow.com/questions/838384/reorder-vector-using-a-vector-of-indices/1267878#1267878
//  to reorder a vector 'value_iterator' based on order vector 'order_iterator'
// */
// template< typename order_iterator, typename value_iterator >
// void reorder( order_iterator order_begin, order_iterator order_end, value_iterator v )  {
//     typedef typename std::iterator_traits< value_iterator >::value_type value_t;
//     typedef typename std::iterator_traits< order_iterator >::value_type index_t;
//     typedef typename std::iterator_traits< order_iterator >::difference_type diff_t;
//
//     diff_t remaining = order_end - 1 - order_begin;
//     for ( index_t s = index_t(), d; remaining > 0; ++ s ) {
//         for ( d = order_begin[s]; d > s; d = order_begin[d] ) ;
//         if ( d == s ) {
//             -- remaining;
//             value_t temp = v[s];
//             while ( d = order_begin[d], d != s ) {
//               std::swap( temp, v[d] );
//                 -- remaining;
//             }
//             v[s] = temp;
//         }
//     }
// }
