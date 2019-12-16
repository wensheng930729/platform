package com.bee.platform.datadriver.service;

import com.baomidou.mybatisplus.plugins.pagination.Pagination;
import com.bee.platform.common.entity.AuthPlatformUserInfo;
import com.bee.platform.common.entity.ResponseResult;
import com.bee.platform.common.entity.UserInfo;
import com.bee.platform.datadriver.dto.ErpRepositoryListDTO;
import com.bee.platform.datadriver.entity.ErpRepository;
import com.bee.platform.datadriver.rq.ErpRepositoryAddRQ;
import com.bee.platform.datadriver.rq.ErpRepositoryDeleteRQ;
import com.bee.platform.datadriver.rq.ErpRepositoryEnableRQ;
import com.bee.platform.datadriver.rq.ErpRepositoryUpdataRQ;
import com.baomidou.mybatisplus.service.IService;
import org.springframework.transaction.annotation.Transactional;

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

    @Transactional(rollbackFor = Exception.class)
	ResponseResult query(Pagination pagination,Integer companyId);

    /**
     * 仓库档案启用还是禁用
     * @param userInfo
     * @param id
     * @return
     */
	ResponseResult updateErpRepositoryEnable(AuthPlatformUserInfo userInfo, ErpRepositoryEnableRQ enableRQ);

	/**
	 * 删除
	 * @param userInfo
	 * @param deleteRQ
	 * @return
	 */
	ResponseResult delete(AuthPlatformUserInfo userInfo,ErpRepositoryDeleteRQ deleteRQ);

	/**
	 * 添加
	 * @param userInfo
	 * @param addRQ
	 * @return
	 */
	ResponseResult add(AuthPlatformUserInfo userInfo, ErpRepositoryAddRQ addRQ);

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
	ResponseResult update(AuthPlatformUserInfo userInfo, ErpRepositoryUpdataRQ rq);

	List<ErpRepositoryListDTO> getRepositoryList(AuthPlatformUserInfo userInfo,String sysToken);

	/**
	 * 根据状态查询仓库档案
	 * @param userInfo
	 * @param pagination
	 * @param companyId
	 * @param status
	 * @return
	 * 
	 */
	ResponseResult getStatus(Pagination pagination, Integer companyId, Integer status);



	
}
