package com.bee.platform.datadriver.service;

import com.baomidou.mybatisplus.plugins.pagination.Pagination;
import com.baomidou.mybatisplus.service.IService;
import com.bee.platform.common.entity.AuthPlatformUserInfo;
import com.bee.platform.common.entity.ResponseResult;
import com.bee.platform.datadriver.dto.ErpSaleStatementDTO;
import com.bee.platform.datadriver.dto.ErpSaleStatementDetailDTO;
import com.bee.platform.datadriver.entity.ErpSaleStatement;
import com.bee.platform.datadriver.rq.ErpSaleStatementSelectRQ;
import com.bee.platform.datadriver.rq.SaleStatementRQ;

import java.util.List;

/**
 * <p>
 * 销售结算单 服务类
 * </p>
 *
 * @author jie.chen123
 * @since 2019-05-27
 */
public interface ErpSaleStatementService extends IService<ErpSaleStatement> {

    /**
     * @Description 新增销售结算
     * @Param saleStatementRQ
     * @Date 2019/5/31 16:02
     * @Author xin.huang
     * @Return
     */
    ResponseResult<Integer> add(AuthPlatformUserInfo simpleUserInfo, SaleStatementRQ saleStatementRQ);

    /**
     * @Description 更新销售结算
     * @Param id
     * @Param saleStatementRQ
     * @Date 2019/5/31 16:54
     * @Author xin.huang
     * @Return
     */
    ResponseResult<Integer> update(AuthPlatformUserInfo simpleUserInfo, SaleStatementRQ saleStatementRQ);

    /**
     * @Description 批量删除销售结算
     * @Param ids
     * @Date 2019/5/31 16:54
     * @Author xin.huang
     * @Return
     */
    ResponseResult<Integer> deleteSaleStatement(Integer id);

    /**
     * @Description 查看销售结算单详情
     * @Param null
     * @Date 2019/6/1 10:06
     * @Author xin.huang
     * @Return
     */
    ResponseResult<ErpSaleStatementDTO> findStatementOrder(Integer id);

    /**
     * @Description 条件查询销售结算单列表
     * @Param saleStatementSelectRQ
     * @Param pagination
     * @Date 2019/5/30 14:45
     * @Author xin.huang
     * @Return
     */
    List<ErpSaleStatementDetailDTO> findSaleStatementOrderList(Integer companyId, ErpSaleStatementSelectRQ saleStatementSelectRQ, Pagination pagination);

    /**
     * @Description 更新销售结算单状态
     * @Param id
     * @Param state
     * @Date 2019/6/1 11:37
     * @Author xin.huang
     * @Return
     */
    ResponseResult<Integer> updateState(AuthPlatformUserInfo simpleUserInfo, Integer id, Integer state);

    /**
     * @Description 根据销售订单id查看销售结算情况
     * @Param id
     * @Date 2019/6/12 18:56
     * @Author xin.huang
     * @Return
     */
    ResponseResult<List<ErpSaleStatementDetailDTO>> getSaleStatementInfo(Integer id);
}
