package com.bee.platform.datadriver.service;

import com.baomidou.mybatisplus.service.IService;
import com.bee.platform.common.entity.Page;
import com.bee.platform.common.entity.ResponseResult;
import com.bee.platform.datadriver.dto.ErpStockDetailDTO;
import com.bee.platform.datadriver.dto.ErpStockSearchListDTO;
import com.bee.platform.datadriver.entity.ErpStock;
import com.bee.platform.datadriver.rq.ErpStockDetailListRQ;
import com.bee.platform.datadriver.rq.ErpStockSearchRQ;

import java.util.List;

/**
 * <p>
 * 库存表 服务类
 * </p>
 *
 * @author chenjie123123
 * @since 2019-06-03
 */
public interface ErpStockService extends IService<ErpStock> {
    /**
     * 条件搜索现存明细
     * @param rq 请求参数
     * @param page 分页对象
     * @param companyId 公司id
     * @return
     */
    ResponseResult<List<ErpStockSearchListDTO>> searchStockByCondition(ErpStockSearchRQ rq, Page page, Integer companyId);
    /**
     * 查询现存明细详情列表
     * @param rq 请求参数
     * @param page 分页对象
     * @return 详情列表
     */
    ResponseResult<List<ErpStockDetailDTO>> getStockDetailByCondition(ErpStockDetailListRQ rq, Page page);
}
