package com.bee.platform.datadriver.service;

import com.baomidou.mybatisplus.plugins.pagination.Pagination;
import com.bee.platform.common.entity.AuthPlatformUserInfo;
import com.bee.platform.common.entity.ResponseResult;
import com.bee.platform.datadriver.dto.ErpRepositoryBoxDTO;
import com.bee.platform.datadriver.dto.ErpRepositoryDetailsDTO;
import com.bee.platform.datadriver.dto.ErpRepositoryListDTO;
import com.bee.platform.datadriver.entity.ErpRepository;
import com.bee.platform.datadriver.rq.ErpRepositoryAddRQ;
import com.bee.platform.datadriver.rq.ErpRepositoryDeleteRQ;
import com.bee.platform.datadriver.rq.ErpRepositoryEnableRQ;
import com.bee.platform.datadriver.rq.ErpRepositoryUpdataRQ;
import com.baomidou.mybatisplus.service.IService;

import java.util.List;

/**
 * <p>
 * 仓库档案 服务类
 * </p>
 *
 * @author jie.chen123
 * @since 2019-05-27
 */
public interface ErpRepositoryService extends IService<ErpRepository> {

	/**
	 * 列表
	 * @param pagination
	 * @param orgId
	 * @param status
	 * @return
	 */
	ResponseResult<List<ErpRepositoryListDTO>> query(Pagination pagination, Integer orgId, Integer status);

    /**
     * 仓库档案启用还是禁用
     * @param userInfo
     * @param enableRQ
     * @return
     */
	ResponseResult<Integer> updateErpRepositoryEnable(AuthPlatformUserInfo userInfo, ErpRepositoryEnableRQ enableRQ);

	/**
	 * 删除
	 * @param userInfo
	 * @param deleteRQ
	 * @return
	 */
	ResponseResult<Integer> delete(AuthPlatformUserInfo userInfo,ErpRepositoryDeleteRQ deleteRQ);

	/**
	 * 添加
	 * @param userInfo
	 * @param addRQ
	 * @return
	 */
	ResponseResult<Integer> add(AuthPlatformUserInfo userInfo, ErpRepositoryAddRQ addRQ);

	/**
	 * 编辑需要返回的数据
	 * @param id
	 * @return
	 */
	ResponseResult get(int id);

	/**
	 * 编辑
	 * @param userInfo
	 * @param rq
	 * @return
	 */
	ResponseResult<Integer> update(AuthPlatformUserInfo userInfo, ErpRepositoryUpdataRQ rq);

	List<ErpRepositoryBoxDTO> getRepositoryList(AuthPlatformUserInfo userInfo, String sysToken, List<String> categoryList);

	/**
	 * 根据状态查询仓库档案
	 * @param pagination
	 * @param companyId
	 * @param status
	 * @return
	 * 
	 */
	ResponseResult<List<ErpRepositoryDetailsDTO>> getStatus(Pagination pagination, Integer companyId, Integer status);



	
}
