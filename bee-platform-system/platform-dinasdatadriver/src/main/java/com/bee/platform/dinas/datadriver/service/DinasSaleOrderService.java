package com.bee.platform.dinas.datadriver.service;

import com.baomidou.mybatisplus.plugins.pagination.Pagination;
import com.bee.platform.common.entity.AuthPlatformUserInfo;
import com.bee.platform.common.entity.ResCodeEnum;
import com.bee.platform.common.entity.ResponseResult;
import com.bee.platform.dinas.datadriver.dto.SaleOrderDTO;
import com.bee.platform.dinas.datadriver.dto.SaleOrderDetailDTO;
import com.bee.platform.dinas.datadriver.dto.SaleOrderListDTO;
import com.bee.platform.dinas.datadriver.entity.DinasSaleOrder;
import com.baomidou.mybatisplus.service.IService;
import com.bee.platform.dinas.datadriver.rq.SaleBatchDeleteRQ;
import com.bee.platform.dinas.datadriver.rq.SaleOrderListRQ;
import com.bee.platform.dinas.datadriver.rq.SaleOrderRQ;

import java.util.List;

/**
 * <p>
 * 销售合同 服务类
 * </p>
 *
 * @author huangxin123
 * @since 2019-08-13
 */
public interface DinasSaleOrderService extends IService<DinasSaleOrder> {

    /**
     * @descriptin 添加销售合同
     * @author xin.huang
     * @param userInfo
     * @param rq
     * @date 2019/8/14
     * @return
     */
    ResponseResult<Integer> add(AuthPlatformUserInfo userInfo, SaleOrderRQ rq);

    /**
     * @descriptin 编辑销售合同
     * @author xin.huang
     * @param userInfo
     * @param rq
     * @date 2019/8/14
     * @return
     */
    ResponseResult<Integer> update(AuthPlatformUserInfo userInfo, SaleOrderRQ rq);

    /**
     * @descriptin 根据合同id查询销售合同详情
     * @author xin.huang
     * @param id
     * @date 2019/8/14
     * @return
     */
    ResponseResult<SaleOrderDTO> findSaleOrderInfo(Integer id);

    /**
     * @descriptin 批量删除销售合同
     * @author xin.huang
     * @param rq
     * @date 2019/8/14
     * @return
     */
    ResponseResult<List<Integer>> batchDelete(AuthPlatformUserInfo userInfo, SaleBatchDeleteRQ rq);

    /**
     * @descriptin 查询销售合同列表
     * @author xin.huang
     * @param userInfo
     * @param rq
     * @param pagination
     * @date 2019/8/14
     * @return
     */
    ResponseResult<List<SaleOrderListDTO>> findSaleOrders(AuthPlatformUserInfo userInfo, SaleOrderListRQ rq, Pagination pagination);

    /**
     * @descriptin 根据公司id获取销售合同列表
     * @author xin.huang
     * @param companyId
     * @date 2019/8/15
     * @return
     */
    ResponseResult<List<SaleOrderDTO>> findOrdersByCompanyId(Integer companyId);

    /**
     * @descriptin 根据销售合同id查询当前合同下的产品
     * @author xin.huang
     * @param id
     * @date 2019/8/15
     * @return
     */
    ResponseResult<List<SaleOrderDetailDTO>> findProductsById(Integer id);

    /**
     * @descriptin 根据销售合同id及产品id查询当前产品下的规格
     * @author xin.huang
     * @param orderId
     * @param productId
     * @date 2019/8/15
     * @return
     */
    ResponseResult<List<SaleOrderDetailDTO>> findSpecsByProductId(Integer orderId, Integer productId);

    /**
     * @descriptin 根据产品id及规格id查询调价前价格
     * @author xin.huang
     * @param orderId
     * @param productId
     * @param specId
     * @date 2019/8/15
     * @return
     */
    ResponseResult<SaleOrderDetailDTO> getPriceByProductSpec(Integer orderId, Integer productId, Integer productSpecId);

}
