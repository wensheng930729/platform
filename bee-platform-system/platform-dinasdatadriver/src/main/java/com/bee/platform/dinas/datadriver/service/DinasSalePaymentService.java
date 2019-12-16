package com.bee.platform.dinas.datadriver.service;

import com.baomidou.mybatisplus.plugins.pagination.Pagination;
import com.bee.platform.common.entity.AuthPlatformUserInfo;
import com.bee.platform.common.entity.ResponseResult;
import com.bee.platform.dinas.datadriver.dto.DinasSalePaymentDTO;
import com.bee.platform.dinas.datadriver.dto.SalePaymentListDTO;
import com.bee.platform.dinas.datadriver.entity.DinasSalePayment;
import com.baomidou.mybatisplus.service.IService;
import com.bee.platform.dinas.datadriver.rq.SaleBatchDeleteRQ;
import com.bee.platform.dinas.datadriver.rq.SalePaymentListRQ;
import com.bee.platform.dinas.datadriver.rq.SalePaymentRQ;

import java.util.List;

/**
 * <p>
 * 销售回款 服务类
 * </p>
 *
 * @author huangxin123
 * @since 2019-08-13
 */
public interface DinasSalePaymentService extends IService<DinasSalePayment> {

    /**
     * @descriptin 添加销售回款
     * @author xin.huang
     * @param userInfo
     * @param rq
     * @date 2019/8/14
     * @return
     */
    ResponseResult<Integer> add( AuthPlatformUserInfo userInfo, SalePaymentRQ rq);

    /**
     * @descriptin 编辑销售回款
     * @author xin.huang
     * @param userInfo
     * @param rq
     * @date 2019/8/14
     * @return
     */
    ResponseResult<Integer> update( AuthPlatformUserInfo userInfo, SalePaymentRQ rq);

    /**
     * @descriptin 查询销售回款详情
     * @author xin.huang
     * @param id
     * @date 2019/8/15
     * @return
     */
    ResponseResult<DinasSalePaymentDTO> findInfo(Integer id);

    /**
     * @descriptin 批量删除销售回款
     * @author xin.huang
     * @param ids
     * @date 2019/8/15
     * @return
     */
    ResponseResult<List<Integer>> batchDelete(AuthPlatformUserInfo userInfo, SaleBatchDeleteRQ rq);

    /**
     * @descriptin 查询销售回款列表
     * @author xin.huang
     * @param userInfo
     * @param rq
     * @param pagination
     * @date 2019/8/15
     * @return
     */
    ResponseResult<List<SalePaymentListDTO>> findList(AuthPlatformUserInfo userInfo, SalePaymentListRQ rq, Pagination pagination);

}
