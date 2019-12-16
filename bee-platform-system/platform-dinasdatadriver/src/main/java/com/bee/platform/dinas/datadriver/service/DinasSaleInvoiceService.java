package com.bee.platform.dinas.datadriver.service;

import com.baomidou.mybatisplus.plugins.pagination.Pagination;
import com.bee.platform.common.entity.AuthPlatformUserInfo;
import com.bee.platform.common.entity.ResponseResult;
import com.bee.platform.dinas.datadriver.dto.SaleInvoiceDTO;
import com.bee.platform.dinas.datadriver.entity.DinasSaleInvoice;
import com.baomidou.mybatisplus.service.IService;
import com.bee.platform.dinas.datadriver.rq.SaleBatchDeleteRQ;
import com.bee.platform.dinas.datadriver.rq.SaleInvoiceListRQ;
import com.bee.platform.dinas.datadriver.rq.SaleInvoiceRQ;

import java.util.List;

/**
 * <p>
 * 销售发票 服务类
 * </p>
 *
 * @author huangxin123
 * @since 2019-08-13
 */
public interface DinasSaleInvoiceService extends IService<DinasSaleInvoice> {

    /**
     * @descriptin 添加销售发票
     * @author xin.huang
     * @param userInfo
     * @param rq
     * @date 2019/8/15
     * @return
     */
    ResponseResult<Integer> add(AuthPlatformUserInfo userInfo, SaleInvoiceRQ rq);

    /**
     * @descriptin 编辑销售发票
     * @author xin.huang
     * @param userInfo
     * @param rq
     * @date 2019/8/15
     * @return
     */
    ResponseResult<Integer> update(AuthPlatformUserInfo userInfo, SaleInvoiceRQ rq);

    /**
     * @descriptin 查询销售发票详情
     * @author xin.huang
     * @param id
     * @date 2019/8/15
     * @return
     */
    ResponseResult<SaleInvoiceDTO> findInfo(Integer id);

    /**
     * @descriptin 批量删除销售发票
     * @author xin.huang
     * @param userInfo
     * @param rq
     * @date 2019/8/15
     * @return
     */
    ResponseResult<List<Integer>> batchDelete(AuthPlatformUserInfo userInfo, SaleBatchDeleteRQ rq);

    /**
     * @descriptin 查询销售发票列表
     * @author xin.huang
     * @param userInfo
     * @param rq
     * @param pagination
     * @date 2019/8/15
     * @return
     */
    ResponseResult<List<SaleInvoiceDTO>> findList(AuthPlatformUserInfo userInfo, SaleInvoiceListRQ rq, Pagination pagination);

}
