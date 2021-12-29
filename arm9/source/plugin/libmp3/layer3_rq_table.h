
/*
 * table for requantization
 *
 * rq_table[x].mantissa * 2^(rq_table[x].exponent) = x^(4/3)
 */

/*
static
struct fixedfloat {
  unsigned long mantissa  : 27;
  unsigned short exponent :  5;
} const rq_table[8207] = {
# include "rq_table.dat"
};
*/

static
struct fixedfloat {
  unsigned long mantissa  : 27;
  unsigned short exponent :  5;
} rq_table[8207] = {
//# include "rq_table.dat"
};

static bool rq_table_isLoaded=false;

extern bool layer3_rq_table_isLoaded(void);
extern int layer3_rq_table_GetTableSize(void);
extern void* layer3_rq_table_GetTableData(void);

extern bool layer3_rq_table_isLoaded(void)
{
  return(rq_table_isLoaded);
}

extern int layer3_rq_table_GetTableSize(void)
{
  return(sizeof(rq_table));
}

extern void* layer3_rq_table_GetTableData(void)
{
  rq_table_isLoaded=true;
  return(rq_table);
}

