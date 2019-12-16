package com.bee.platform.dinas.datadriver.service;

import com.baomidou.mybatisplus.service.IService;
import com.bee.platform.common.entity.AuthPlatformUserInfo;
import com.bee.platform.common.entity.Page;
import com.bee.platform.common.entity.ResponseResult;
import com.bee.platform.dinas.datadriver.dto.DinasCustomerBoxDTO;
import com.bee.platform.dinas.datadriver.dto.DinasCustomerDetailDTO;
import com.bee.platform.dinas.datadriver.dto.DinasCustomerListDTO;
import com.bee.platform.dinas.datadriver.entity.DinasCustomer;
import com.bee.platform.dinas.datadriver.rq.DinasCustomerAddRQ;
import com.bee.platform.dinas.datadriver.rq.DinasCustomerQueryRQ;
import com.bee.platform.dinas.datadriver.rq.DinasCustomerUpdateRQ;

import java.util.List;

/**
 * <p>
 * 客户表 服务类
 * </p>
 *
 * @author liliang123
 * @since 2019-08-13
 */
public interface DinasCustomerService extends IService<DinasCustomer> {

    /**
     * 添加客户
     *
     * @param userInfo
     * @param rq
     * @return
     */
    public ResponseResult addCustomer(AuthPlatformUserInfo userInfo, DinasCustomerAddRQ rq);

    /**
     * 批量删除
     *
     *
     * @param type
     * @param ids
     * @return
     */
    public ResponseResult deleteCustomer(AuthPlatformUserInfo userInfo, Integer type, List<Integer> ids);

    /**
     * 编辑
     *
     * @param userInfo
     * @param rq
     * @return
     */
    public ResponseResult updateCustomer(AuthPlatformUserInfo userInfo, DinasCustomerUpdateRQ rq);

    /**
     * 切换状态
     *
     * @param userInfo
     * @param id
     * @param status
     * @return
     */
    public ResponseResult switchCustomer(AuthPlatformUserInfo userInfo, Integer id, Integer status);

    /**
     * 详情
     *
     * @param id
     * @return
     */
    public ResponseResult<DinasCustomerDetailDTO> getCustomerDetail(Integer id);

    /**
     * 客户列表
     *
     * @param orgId
     * @param page
     * @param rq
     * @return
     */
    public ResponseResult<List<DinasCustomerListDTO>> getCustomerList(Page page, Integer orgId, DinasCustomerQueryRQ rq);

    /**
     * 下拉框--根据类型查询客户
     *
     * @param orgId
     * @param type
     * @return
     */
    public ResponseResult<List<DinasCustomerBoxDTO>> getCustomerByType(Integer orgId, Integer type);

    /**
     * 下拉框--根据类型和产品查询客户
     *
     * @param productId
     * @param type
     * @return
     */
    public ResponseResult<List<DinasCustomerBoxDTO>> getCustomerByTypeAndProduct(Integer orgId, Integer productId, Integer type);
}
