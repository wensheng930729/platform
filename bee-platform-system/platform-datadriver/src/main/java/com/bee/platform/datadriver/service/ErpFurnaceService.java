package com.bee.platform.datadriver.service;

import com.baomidou.mybatisplus.plugins.pagination.Pagination;
import com.baomidou.mybatisplus.service.IService;
import com.bee.platform.common.entity.AuthPlatformUserInfo;
import com.bee.platform.common.entity.Page;
import com.bee.platform.common.entity.ResponseResult;
import com.bee.platform.datadriver.dto.ErpFurnaceBoxDTO;
import com.bee.platform.datadriver.dto.ErpFurnaceListDTO;
import com.bee.platform.datadriver.dto.ErpFurnaceOneDTO;
import com.bee.platform.datadriver.entity.ErpFurnace;
import com.bee.platform.datadriver.rq.ErpFurnacaAddRQ;
import com.bee.platform.datadriver.rq.ErpFurnacaUpdateRQ;

import java.util.List;

/**
 * <p>
 * 炉子档案 服务类
 * </p>
 *
 * @author jie.chen123
 * @since 2019-05-27
 */
public interface ErpFurnaceService extends IService<ErpFurnace> {


    /**
     * 增加erp炉子
     *
     * @return
     */
    public ResponseResult<Integer> addFurnace(ErpFurnacaAddRQ rq, AuthPlatformUserInfo userInfo);

    /**
     * 删除erp炉子
     *
     * @return
     */
    public ResponseResult<Integer> deleteFurnace(Integer id, AuthPlatformUserInfo userInfo);

    /**
     * 修改erp炉子
     *
     * @return
     */
    public ResponseResult<Integer> updateFurnace(ErpFurnacaUpdateRQ rq, AuthPlatformUserInfo userInfo);

    /**
     * 根据当前炉子状态 修改erp炉子启用/禁用状态
     *
     * @return
     */
    public ResponseResult<Integer> updateFurnaceStatus(Integer id, Integer status, AuthPlatformUserInfo userInfo);

    /**
     * 根据id查询炉子信息
     * @param id
     * @return
     */
    ResponseResult<ErpFurnaceOneDTO> getById(Integer id);
    /**
     * 查询炉子档案列表
     *
     * @param companyId
     * @return
     */
    ResponseResult<List<ErpFurnaceListDTO>> query(Integer companyId, Pagination pagination);
    /**
     * 查询当前用户企业下的炉号
     * @param userInfo
     * @return
     */
    ResponseResult<List<ErpFurnaceBoxDTO>> queryFurnaceNum(AuthPlatformUserInfo userInfo);

    /**
     * 查询炉子列表
     * @param userInfo
     * @param sysToken
     * @return
     */
    List<ErpFurnaceBoxDTO> getFurnaceList(AuthPlatformUserInfo userInfo,String sysToken);


}
