package com.bee.platform.datadriver.service;

import com.baomidou.mybatisplus.plugins.pagination.Pagination;
import com.bee.platform.common.entity.AuthPlatformUserInfo;
import com.bee.platform.common.entity.ResponseResult;
import com.bee.platform.datadriver.entity.ErpCode;
import com.bee.platform.common.entity.Page;
import com.bee.platform.datadriver.dto.ErpCrmRankToCommercialDTO;
import com.bee.platform.datadriver.dto.ErpCrmSalesRankDTO;
import com.bee.platform.datadriver.dto.ErpCrmCommercialOpportunityDTO;
import com.bee.platform.datadriver.entity.ErpCrmCommercialOpportunity;
import com.baomidou.mybatisplus.service.IService;
import com.bee.platform.datadriver.rq.CommercialOpportunityQueryRQ;
import com.bee.platform.datadriver.rq.ErpCrmCommercialOpportunityAddRQ;
import com.bee.platform.datadriver.rq.ErpCrmCommercialOpportunityDeleteRQ;
import com.bee.platform.datadriver.rq.ErpCrmCommercialOpportunityUpdateRQ;
import org.apache.poi.ss.usermodel.Workbook;

import java.util.List;

import javax.servlet.http.HttpServletRequest;

/**
 * <p>
 * 商机信息 服务类
 * </p>
 *
 * @author chenjie123123
 * @since 2019-06-24
 */
public interface ErpCrmCommercialOpportunityService extends IService<ErpCrmCommercialOpportunity> {

    /**
     * 分页查询商机信息
     * @param pagination
     * @param rq
     * @param companyId
     * @return
     */
    List<ErpCrmCommercialOpportunity> listCommercialOpportunity(Pagination pagination, CommercialOpportunityQueryRQ rq, Integer companyId,AuthPlatformUserInfo userInfo);

    /**
     * 查询客户
     * @return
     */
	List<ErpCode> getCustomerType(String customertype);

	/**
	 * 查询客户当前的进度
	 * @param code
	 * @return
	 */
	ResponseResult<List<ErpCode>> getcustomerCurrentStage(String code);

	/**
	 * 根据当前人登录id查询当前商机信息
	 * @param userInfo
	 * @return
	 */
	ResponseResult<ErpCrmCommercialOpportunity> getCommercialOpportunityUserId(AuthPlatformUserInfo userInfo);

    ResponseResult<List<ErpCrmSalesRankDTO>> getSellerSalesRank(Integer id, Integer orgId, Page page);

    ResponseResult<List<ErpCrmSalesRankDTO>> getManagerSalesRank(Integer orgId, Page page);

    ResponseResult<List<ErpCrmRankToCommercialDTO>> getCrmRankToCommercial(Integer saleUserId, Integer type, Integer orgId, Page page);

    /**
     *
     * @param id
     * @return
     */
    ResponseResult<ErpCrmCommercialOpportunityDTO> getCommercialOpportunity(Integer id);

    /**
     *
     * @param rq
     * @param userInfo
     * @return
     */
    ResponseResult<Integer> addCommercialOpportunity(ErpCrmCommercialOpportunityAddRQ rq, AuthPlatformUserInfo userInfo,Integer companyId);

    ResponseResult<Integer> updateCommercialOpportunity(ErpCrmCommercialOpportunityUpdateRQ rq, AuthPlatformUserInfo userInfo,Integer companyId);


    Workbook exportSalesRank(Integer companyId);

	Workbook exportBOC(Integer companyId,CommercialOpportunityQueryRQ rq,AuthPlatformUserInfo userInfo);

	ResponseResult<ErpCrmCommercialOpportunity> deleted(AuthPlatformUserInfo userInfo,
			ErpCrmCommercialOpportunityDeleteRQ deleteRQ);
}
