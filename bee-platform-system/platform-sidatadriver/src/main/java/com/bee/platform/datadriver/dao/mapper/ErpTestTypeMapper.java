package com.bee.platform.datadriver.dao.mapper;

import com.baomidou.mybatisplus.plugins.pagination.Pagination;
import com.bee.platform.datadriver.dto.ErpTestTypeDTO;
import com.bee.platform.datadriver.entity.ErpTestReport;
import com.bee.platform.datadriver.entity.ErpTestType;
import com.baomidou.mybatisplus.mapper.BaseMapper;
import com.baomidou.mybatisplus.mapper.EntityWrapper;
import com.bee.platform.datadriver.rq.TestReportTypeRQ;

import java.util.List;

/**
 * <p>
 * 化验类型 Mapper 接口
 * </p>
 *
 * @author jie.chen123
 * @since 2019-05-27
 */
public interface ErpTestTypeMapper extends BaseMapper<ErpTestType> {

    /**
     * 化验类型列表查询接口
     * @param rq
     * @return
     */
    List<ErpTestTypeDTO> listErpTestType(TestReportTypeRQ rq, Pagination pagination);
}
