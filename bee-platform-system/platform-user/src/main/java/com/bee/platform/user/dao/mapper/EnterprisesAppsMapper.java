package com.bee.platform.user.dao.mapper;

import com.bee.platform.common.entity.ResponseResult;
import com.bee.platform.user.dto.AppDetailDTO;
import com.bee.platform.user.dto.AppListDTO;
import com.bee.platform.user.dto.EnterprisesAppsDTO;
import com.bee.platform.user.dto.EnterprisesAppsOpenedDTO;
import com.bee.platform.user.entity.EnterprisesApps;

import java.util.List;

import org.apache.ibatis.annotations.Param;

import com.baomidou.mybatisplus.mapper.BaseMapper;
import com.baomidou.mybatisplus.plugins.pagination.Pagination;

/**
 * <p>
 *  Mapper 接口
 * </p>
 *
 * @author jie.zhang123
 * @since 2019-03-04
 */
public interface EnterprisesAppsMapper extends BaseMapper<EnterprisesApps> {

    /**
     * 获取已开通产品列表
     * @param orgId
     * @return
     */
    List<AppListDTO> listOpenedApp(Integer orgId);

    /**
     * 获取未开通产品列表
     * @param orgId
     * @return
     */
    List<AppListDTO> listNotOpenedApp(Integer orgId);

    /**
     * 查询已开通的产品及角色
     * @return
     */
	List<EnterprisesAppsOpenedDTO> queryOpendAppRoles(Integer orgId);

	/**
	 * 审核状态及产品名称查询
	 * @param auditState
	 * @param content
	 * @param pagination
	 * @return
	 */
	List<EnterprisesAppsDTO> queryAppsByAppName(@Param("auditState") Integer auditState,@Param("content") String content, Pagination pagination);

	/**
	 * 产品名称查询所有
	 * @param content
	 * @param pagination
	 * @return
	 */
	List<EnterprisesAppsDTO> getAllByAppName(@Param("content") String content, Pagination pagination);

	/**
	 * 公司名称查询所有
	 * @param content
	 * @param pagination
	 * @return
	 */
	List<EnterprisesAppsDTO> getAllByOrgName(@Param("content") String content, Pagination pagination);

	/**
	 * 审核状态及公司名称查询
	 * @param auditState
	 * @param content
	 * @param pagination
	 * @return
	 */
	List<EnterprisesAppsDTO> queryAppsByOrgName(@Param("auditState") Integer auditState, @Param("content") String content, Pagination pagination);
}
