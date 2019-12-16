package com.bee.platform.datadriver.service.impl;

import cn.afterturn.easypoi.excel.ExcelExportUtil;
import cn.afterturn.easypoi.excel.entity.ExportParams;
import com.baomidou.mybatisplus.mapper.EntityWrapper;
import com.baomidou.mybatisplus.mapper.Wrapper;
import com.baomidou.mybatisplus.plugins.pagination.Pagination;
import com.baomidou.mybatisplus.service.impl.ServiceImpl;
import com.baomidou.mybatisplus.toolkit.CollectionUtils;
import com.bee.platform.common.entity.AuthPlatformUserInfo;
import com.bee.platform.common.entity.Page;
import com.bee.platform.common.entity.ResCodeEnum;
import com.bee.platform.common.entity.ResponseResult;
import com.bee.platform.common.enums.Status;
import com.bee.platform.common.exception.BusinessException;
import com.bee.platform.common.exception.ExceptionMessageEnum;
import com.bee.platform.common.service.JedisService;
import com.bee.platform.common.service.RegionService;
import com.bee.platform.common.utils.BeanUtils;
import com.bee.platform.common.utils.PageUtils;
import com.bee.platform.customer.dao.mapper.AuthContactMapper;
import com.bee.platform.customer.dao.mapper.AuthCustomerContactMapper;
import com.bee.platform.customer.dao.mapper.AuthCustomerMapper;
import com.bee.platform.customer.entity.AuthContact;
import com.bee.platform.customer.entity.AuthCustomer;
import com.bee.platform.customer.entity.AuthCustomerContact;
import com.bee.platform.datadriver.dao.mapper.*;
import com.bee.platform.datadriver.dto.*;
import com.bee.platform.datadriver.entity.ErpCode;
import com.bee.platform.datadriver.entity.ErpCrmCommercialOpportunity;
import com.bee.platform.datadriver.entity.ErpCrmDepartmentAdmin;
import com.bee.platform.datadriver.entity.ErpCrmProductMarketing;
import com.bee.platform.datadriver.enums.EnumErpCode;
import com.bee.platform.datadriver.rq.*;
import com.bee.platform.datadriver.service.CommonService;
import com.bee.platform.datadriver.service.ErpCrmCommercialOpportunityService;
import com.bee.platform.datadriver.service.ErpCrmProductMarketingService;
import com.bee.platform.user.service.feign.AuthPlatformUserEnterpriseFeignClient;
import com.bee.platform.user.service.feign.AuthUserFeignClient;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.StringUtils;
import org.apache.poi.ss.usermodel.Workbook;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.util.ObjectUtils;

import java.util.*;
import java.util.stream.Collectors;

/**
 * <p>
 * 商机信息 服务实现类
 * </p>
 *
 * @author chenjie123123
 * @since 2019-06-24
 */

@Slf4j
@Service
public class ErpCrmCommercialOpportunityServiceImpl extends ServiceImpl<ErpCrmCommercialOpportunityMapper, ErpCrmCommercialOpportunity> implements ErpCrmCommercialOpportunityService {

    @Autowired
    private ErpCrmCommercialOpportunityMapper crmCommercialOpportunityMapper;
    @Autowired
    private ErpCrmProductMarketingMapper productMarketingMapper;
    @Autowired
    private ErpCrmProductMarketingService erpCrmProductMarketingService;
    @Autowired
    private ErpCodeMapper erpCodeMapper;
    @Autowired
    private AuthCustomerMapper customerMapper;
    @Autowired
    private JedisService jedisService;
    @Autowired
    private AuthCustomerContactMapper customerContactMapper;
    @Autowired
    private AuthContactMapper contactMapper;
    @Autowired
    private CommonMapper commonMapper;
    @Autowired
    private RegionService regionService;
    @Autowired
    private ErpCrmDepartmentAdminMapper erpCrmDepartmentAdminMapper;
    @Autowired
    private AuthPlatformUserEnterpriseFeignClient authPlatformUserEnterpriseFeignClient;
    @Autowired
    private AuthUserFeignClient userInfoFeignClient;
    @Autowired
    private CommonService commonService;

    private static Integer ZERO = 0;

    /**
     * 分页查询商机信息
     *
     * @param pagination
     * @param rq
     * @param companyId
     * @return
     */
    @Override
    public List<ErpCrmCommercialOpportunity> listCommercialOpportunity(Pagination pagination, CommercialOpportunityQueryRQ rq, Integer companyId, AuthPlatformUserInfo userInfo) {
        Map<String, Object> paramMap = new HashMap<>();

        // 用户不是企业管理员
        if (!userInfo.getManagerType().equals(1)) {
            ArrayList<Object> userIdList = new ArrayList<>();
            Integer userId = userInfo.getId();
            //查询当前登录用户的在  部门和负责人关联表的部门
            List<ErpCrmDepartmentAdmin> erpCrmDepartmentAdminList = erpCrmDepartmentAdminMapper.selectList(new EntityWrapper<>(new ErpCrmDepartmentAdmin()
                    .setUserId(userInfo.getId())
                    .setEnterpriseId(companyId)));
            userIdList.add(userId);
            // 未建立部门管理人的对应关系，则执行查看下级部门权限的逻辑
            if (CollectionUtils.isEmpty(erpCrmDepartmentAdminList)) {
                ResponseResult<Set<Integer>> responseResult = userInfoFeignClient.subordinates(companyId, userId);
                if (ResCodeEnum.SUCCESS.getCode().equals(responseResult.getCode())) {
                    Set<Integer> subordinates = responseResult.getObject();
                    userIdList.addAll(subordinates);
                }
            } else {
                List<Integer> departments = erpCrmDepartmentAdminList.stream().map(o -> o.getDepartmentId()).collect(Collectors.toList());
                ResponseResult<Set<Integer>> responseResult = authPlatformUserEnterpriseFeignClient.findDepartmentIdAndEnterpriseId(departments, companyId);
                if (ResCodeEnum.SUCCESS.getCode().equals(responseResult.getCode())) {
                    userIdList.addAll(responseResult.getObject());
                }
            }
            paramMap.put("userIdList", userIdList);
        }

        paramMap.put("companyId", companyId);

        if (!StringUtils.isEmpty(rq.getCustomerName())) {
            paramMap.put("customerName", rq.getCustomerName());
        }
        if (!StringUtils.isEmpty(rq.getCustomerType())) {
            paramMap.put("customerType", rq.getCustomerType());
        }
        if (!StringUtils.isEmpty(rq.getPhase())) {
            paramMap.put("phase", rq.getPhase());
        }
        if (!StringUtils.isEmpty(rq.getSaleUserName())) {
            paramMap.put("saleUserName", rq.getSaleUserName());
        }

        List<ErpCrmCommercialOpportunity> list = crmCommercialOpportunityMapper.selectByList(paramMap, pagination);
        return list;
    }


    /**
     * 查询所有客户类型
     */
    @Override
    public List<ErpCode> getCustomerType(String customerType) {
        List<ErpCode> list = erpCodeMapper.selectList(new EntityWrapper<>(new ErpCode().setCode(customerType)));
        return list;
    }

    /**
     * 查询客户当前阶段
     */
    @Override
    public ResponseResult<List<ErpCode>> getcustomerCurrentStage(String code) {
        List<ErpCode> list = erpCodeMapper.selectList(new EntityWrapper<>(new ErpCode().setCode(code)));
        if (CollectionUtils.isEmpty(list)) {
            return ResponseResult.buildResponseResult(ResCodeEnum.NO_DATA);
        }
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, list);
    }

    /**
     * 查询当前销售员的客户信息
     */
    @Override
    public ResponseResult<ErpCrmCommercialOpportunity> getCommercialOpportunityUserId(AuthPlatformUserInfo userInfo) {
        ErpCrmCommercialOpportunity erpCrmCommercialOpportunity = crmCommercialOpportunityMapper.selectOne(new ErpCrmCommercialOpportunity().setSaleUserId(userInfo.getId()));
        if (Objects.isNull(erpCrmCommercialOpportunity)) {
            return ResponseResult.buildResponseResult(ResCodeEnum.NO_DATA);
        }
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, erpCrmCommercialOpportunity);
    }


    @Override
    public ResponseResult<List<ErpCrmSalesRankDTO>> getSellerSalesRank(Integer userId, Integer orgId, Page page) {
        Pagination pagination = PageUtils.transFromPage(page);

        List<ErpCrmSalesRankDTO> dto = crmCommercialOpportunityMapper.getSalesRank(orgId, pagination);
        dto.forEach(
                o -> {
                    if (userId.equals(o.getSaleUserId())) {
                        o.setViewMark(true);
                    }
                }
        );


        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, dto, PageUtils.transToPage(pagination));
    }


    @Override
    public ResponseResult<List<ErpCrmSalesRankDTO>> getManagerSalesRank(Integer orgId, Page page) {
        Pagination pagination = PageUtils.transFromPage(page);

        List<ErpCrmSalesRankDTO> dto = crmCommercialOpportunityMapper.getSalesRank(orgId, pagination);
        dto.forEach(o -> o.setViewMark(true));

        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, dto, PageUtils.transToPage(pagination));

    }


    @Override
    public ResponseResult<List<ErpCrmRankToCommercialDTO>> getCrmRankToCommercial(Integer saleUserId, Integer type, Integer orgId, Page page) {
        Pagination pagination = PageUtils.transFromPage(page);
        Wrapper<ErpCrmCommercialOpportunity> wrapper = new EntityWrapper<ErpCrmCommercialOpportunity>().eq("deleted", 0)
                .eq("sale_user_id", saleUserId).eq("company_id", orgId).orderBy("create_time", false);

        switch (type) {
            case 0:
                wrapper.eq("phase", "reserve_customers");
                break;
            case 1:
                wrapper.eq("phase", "visiting_customers");
                break;
            case 2:
                wrapper.eq("phase", "promoting_customers");
                break;
            case 3:
                wrapper.eq("phase", "transaction_clients");
                break;
            default:
                log.info("传入的类型不匹配 type为：" + type);
                return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, new ArrayList<>(), PageUtils.transToPage(pagination));

        }

        List<ErpCrmCommercialOpportunity> list = baseMapper.selectPage(pagination, wrapper);
        List<ErpCrmRankToCommercialDTO> dto = BeanUtils.assemble(ErpCrmRankToCommercialDTO.class, list);

        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, dto, PageUtils.transToPage(pagination));

    }


    /**
     * 根据id查询商机信息
     *
     * @param id
     * @return
     */
    @Override
    public ResponseResult<ErpCrmCommercialOpportunityDTO> getCommercialOpportunity(Integer id) {
        ErpCrmCommercialOpportunityDTO crmCommercialOpportunityDTO = new ErpCrmCommercialOpportunityDTO();
        // 查询商机基本信息
        ErpCrmCommercialOpportunity crmCommercialOpportunity = crmCommercialOpportunityMapper.selectById(new ErpCrmCommercialOpportunity()
                .setId(id).setDeleted(Status.FALSE.getKey()));
        BeanUtils.copyProperties(crmCommercialOpportunity, crmCommercialOpportunityDTO);
        // 查询商机营销产品信息
        List<ErpCrmProductMarketing> productMarketings = productMarketingMapper.selectList(new EntityWrapper<>(new ErpCrmProductMarketing()
                .setCommercialId(id).setDeleted(Status.FALSE.getKey())));
        crmCommercialOpportunityDTO.setProductMarketingDTOList(BeanUtils.assemble(ErpCrmProductMarketingDTO.class, productMarketings));

        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, crmCommercialOpportunityDTO);
    }

    /**
     * 新增商机信息
     *
     * @param rq
     * @param userInfo
     * @return
     */
    @Override
    @Transactional(rollbackFor = Exception.class)
    public ResponseResult<Integer> addCommercialOpportunity(ErpCrmCommercialOpportunityAddRQ rq, AuthPlatformUserInfo userInfo, Integer companyId) {
        // 校验客户名称唯一性
        ErpCrmCommercialOpportunity check = crmCommercialOpportunityMapper.selectOne(new ErpCrmCommercialOpportunity()
                .setCustomerName(rq.getCustomerName()).setDeleted(Status.FALSE.getKey()));

        if (!ObjectUtils.isEmpty(check)) {
            //return ResponseResult.buildResponseResult(ResCodeEnum.CRM_CUSTOMER_NAME_REPEAT,"该商机客户已被" + check.getSaleUserName() + "优先建立，如有疑问您联系事业部总经理");
            throw new BusinessException(ResCodeEnum.FAILED, "该商机客户已被" + check.getSaleUserName() + "优先建立，如有疑问您联系事业部总经理");
        }
        Integer userId = userInfo.getId();
        String companyName = commonMapper.getCompanyNameById(companyId);
        Date time = new Date();
        Integer customerId = null;
        Integer contactId = null;

        // 成交后更新客户档案
        if ("transaction_clients".equals(rq.getPhase())) {
            String redisKey = "customer" + companyId;
            // 客户编码
            int customerCode = commonService.getRedisCode(redisKey);
            String cusNo = commonService.getCode(companyId + "", 4)
                    + commonService.getCode(customerCode + "", 4);
            // redis序号自增
            jedisService.set(redisKey, (customerCode + 1) + "", 0);

            AuthCustomer customer = new AuthCustomer()
                    .setEnterpriseId(companyId)
                    .setCusNo(cusNo)
                    .setCusName(rq.getCustomerName())
                    .setDeleted(Status.FALSE.getKey())
                    .setStatus(Status.TRUE.getKey())
                    .setOperateId(userInfo.getId())
                    .setCreateTime(new Date());
            if (customerMapper.insert(customer) < ZERO) {
                log.error("更新客户档案失败,调用{}类{}方法出错", "ErpCrmCommercialOpportunityServiceImpl", "addCommercialOpportunity()");
                throw new BusinessException(ResCodeEnum.SAVE_FAIL, ExceptionMessageEnum.UPDATE_CUSTOMER_FAILED);
            }
            customerId = customer.getId();
            // 添加客户联系人
            AuthContact contact = new AuthContact().setDeleted(Status.FALSE.getKey()).setName(rq.getContactName())
                    .setPhone(rq.getContactPhone()).setStatus(Status.TRUE.getKey()).setCreateTime(new Date());
            if (contactMapper.insert(contact) < ZERO) {
                log.error("更新客户档案联系人失败,调用{}类{}方法出错", "ErpCrmCommercialOpportunityServiceImpl", "addCommercialOpportunity()");
                throw new BusinessException(ResCodeEnum.SAVE_FAIL, ExceptionMessageEnum.UPDATE_CUSTOMER_CONTACT_FAILED);
            }
            contactId = contact.getId();
            // 添加客户联系人关联
            if (customerContactMapper.insert(new AuthCustomerContact()
                    .setCustomerId(customer.getId()).setContactId(contact.getId())
                    .setDeleted(Status.FALSE.getKey()).setStatus(Status.TRUE.getKey()).setOperateId(userId)
                    .setCreateUser(userId.toString()).setCreateTime(new Date())) < ZERO) {
                log.error("更新客户档案联系人关联失败,调用{}类{}方法出错", "ErpCrmCommercialOpportunityServiceImpl", "addCommercialOpportunity()");
                throw new BusinessException(ResCodeEnum.SAVE_FAIL, ExceptionMessageEnum.UPDATE_CUSTOMER_CONTACT_RELATED_FAILED);
            }
        }


        ErpCrmCommercialOpportunity m = BeanUtils.copyProperties(rq, ErpCrmCommercialOpportunity.class);
        // 获取详细地址
        String address = regionService.assembleFullAddress(rq.getRegionid(), rq.getStreet());
        m.setDeleted(Status.FALSE.getKey())
                .setCreateUser(userId).setCreateTime(time)
                .setCompanyId(companyId)
                .setCustomerId(customerId)
                .setContactId(contactId)
                .setCompanyName(companyName)
                .setAddress(address);

        if (crmCommercialOpportunityMapper.insert(m) <= ZERO) {
            log.error("新增商机信息失败,调用{}类{}方法出错", "ErpCrmCommercialOpportunityServiceImpl", "addCommercialOpportunity()");
            throw new BusinessException(ResCodeEnum.SAVE_FAIL, ExceptionMessageEnum.CRM_COMMERCIAL_OPPORTUNITY_ADD_FAILED);
        }

        List<ErpCrmProductMarketingAddRQ> addDRQList = rq.getProductMarketingAddRQList();
        if (!CollectionUtils.isEmpty(addDRQList)) {
            List<ErpCrmProductMarketing> p = BeanUtils.assemble(ErpCrmProductMarketing.class, addDRQList);
            p.forEach(o -> o.setCommercialId(m.getId()).setCreateTime(time).setCreateUser(userId).setDeleted(Status.FALSE.getKey()));

            if (!erpCrmProductMarketingService.insertBatch(p)) {
                log.error("保存子表商机信息失败");
                log.error("新增商机信息失败,调用{}类{}方法出错", "ErpCrmCommercialOpportunityServiceImpl", "addCommercialOpportunity()");
                throw new BusinessException(ResCodeEnum.SAVE_FAIL, ExceptionMessageEnum.CRM_COMMERCIAL_OPPORTUNITY_ADD_FAILED);
            }
        }


        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, m.getId());
    }

    @Transactional(rollbackFor = Exception.class)
    @Override
    public ResponseResult<Integer> updateCommercialOpportunity(ErpCrmCommercialOpportunityUpdateRQ rq, AuthPlatformUserInfo userInfo, Integer companyId) {
        Integer rqId = rq.getId();
        ErpCrmCommercialOpportunity e = selectById(rqId);
        if (ObjectUtils.isEmpty(e)) {
            log.error("修改的数据不存在 id为：" + rqId);
            log.error("修改商机信息失败,调用{}类{}方法出错", "ErpCrmCommercialOpportunityServiceImpl", "addCommercialOpportunity()");
            throw new BusinessException(ResCodeEnum.SAVE_FAIL, ExceptionMessageEnum.CRM_COMMERCIAL_OPPORTUNITY_ADD_FAILED);
        }

        ErpCrmCommercialOpportunity m = BeanUtils.copyProperties(rq, ErpCrmCommercialOpportunity.class);
        Integer userId = userInfo.getId();
        Date time = new Date();
        Integer customerId = null;
        Integer contactId = null;

        // 第一次 成交后更新客户档案
        if ("transaction_clients".equals(rq.getPhase()) && ObjectUtils.isEmpty(e.getCustomerId()) && ObjectUtils.isEmpty(e.getContactId())) {
            String redisKey = "customer" + companyId;
            // 客户编码
            Integer customerCode = commonService.getRedisCode(redisKey);
            String cusNo = commonService.getCode(companyId + "", 4)
                    + commonService.getCode(customerCode + "", 4);
            // redis序号自增
            jedisService.set(redisKey, (customerCode + 1) + "", 0);

            AuthCustomer customer = new AuthCustomer().setEnterpriseId(companyId).setCusNo(cusNo).setOperateId(userId)
                    .setCusName(rq.getCustomerName()).setDeleted(Status.FALSE.getKey()).setStatus(Status.TRUE.getKey()).setCreateTime(new Date());
            if (customerMapper.insert(customer) < ZERO) {
                log.error("更新客户档案失败,调用{}类{}方法出错", "ErpCrmCommercialOpportunityServiceImpl", "addCommercialOpportunity()");
                throw new BusinessException(ResCodeEnum.SAVE_FAIL, ExceptionMessageEnum.UPDATE_CUSTOMER_FAILED);
            }
            customerId = customer.getId();
            // 添加客户联系人
            AuthContact contact = new AuthContact().setDeleted(Status.FALSE.getKey()).setName(rq.getContactName())
                    .setPhone(rq.getContactPhone()).setStatus(Status.TRUE.getKey()).setCreateTime(new Date());
            if (contactMapper.insert(contact) < ZERO) {
                log.error("更新客户档案联系人失败,调用{}类{}方法出错", "ErpCrmCommercialOpportunityServiceImpl", "addCommercialOpportunity()");
                throw new BusinessException(ResCodeEnum.SAVE_FAIL, ExceptionMessageEnum.UPDATE_CUSTOMER_CONTACT_FAILED);
            }
            contactId = contact.getId();
            // 添加客户联系人关联
            if (customerContactMapper.insert(new AuthCustomerContact()
                    .setCustomerId(customer.getId()).setContactId(contact.getId())
                    .setDeleted(Status.FALSE.getKey()).setStatus(Status.TRUE.getKey()).setOperateId(userId)
                    .setCreateUser(userId.toString()).setCreateTime(new Date())) < ZERO) {
                log.error("更新客户档案联系人关联失败,调用{}类{}方法出错", "ErpCrmCommercialOpportunityServiceImpl", "addCommercialOpportunity()");
                throw new BusinessException(ResCodeEnum.SAVE_FAIL, ExceptionMessageEnum.UPDATE_CUSTOMER_CONTACT_RELATED_FAILED);
            }

        }
        // 已经成交后修改信息
        if ("transaction_clients".equals(rq.getPhase()) && !ObjectUtils.isEmpty(e.getCustomerId()) && !ObjectUtils.isEmpty(e.getContactId())) {

            AuthCustomer customer = new AuthCustomer().setId(e.getCustomerId()).setId(e.getCustomerId()).setOperateId(userId)
                    .setCusName(rq.getCustomerName()).setDeleted(Status.FALSE.getKey()).setStatus(Status.TRUE.getKey()).setUpdateTime(new Date());
            if (customerMapper.updateById(customer) < ZERO) {
                log.error("更新客户档案失败,调用{}类{}方法出错", "ErpCrmCommercialOpportunityServiceImpl", "addCommercialOpportunity()");
                throw new BusinessException(ResCodeEnum.SAVE_FAIL, ExceptionMessageEnum.UPDATE_CUSTOMER_FAILED);
            }
            customerId = customer.getId();
            // 添加客户联系人
            AuthContact contact = new AuthContact().setId(e.getContactId()).setDeleted(Status.FALSE.getKey()).setName(rq.getContactName())
                    .setPhone(rq.getContactPhone()).setStatus(Status.TRUE.getKey()).setCreateTime(new Date());
            if (contactMapper.updateById(contact) < ZERO) {
                log.error("更新客户档案联系人失败,调用{}类{}方法出错", "ErpCrmCommercialOpportunityServiceImpl", "addCommercialOpportunity()");
                throw new BusinessException(ResCodeEnum.SAVE_FAIL, ExceptionMessageEnum.UPDATE_CUSTOMER_CONTACT_FAILED);
            }
            contactId = contact.getId();
        }


        m.setUpdateUser(userId)
                .setUpdateTime(time)
                .setCustomerId(customerId)
                .setContactId(contactId);
        if (crmCommercialOpportunityMapper.updateById(m) <= ZERO) {
            log.error("修改商机信息失败,调用{}类{}方法出错", "ErpCrmCommercialOpportunityServiceImpl", "addCommercialOpportunity()");
            throw new BusinessException(ResCodeEnum.SAVE_FAIL, ExceptionMessageEnum.CRM_COMMERCIAL_OPPORTUNITY_ADD_FAILED);
        }
        // 先删除旧数据
        Wrapper<ErpCrmProductMarketing> wrapper = new EntityWrapper<ErpCrmProductMarketing>().eq("commercial_id", m.getId()).eq("deleted", 0);

        List<ErpCrmProductMarketing> edList = erpCrmProductMarketingService.selectList(wrapper);
        if (!CollectionUtils.isEmpty(edList)) {
            if (!erpCrmProductMarketingService.update(new ErpCrmProductMarketing().setDeleted(1), wrapper)) {
                log.error("删除子表商机信息失败");
                log.error("修改商机信息失败,调用{}类{}方法出错", "ErpCrmCommercialOpportunityServiceImpl", "addCommercialOpportunity()");
                throw new BusinessException(ResCodeEnum.SAVE_FAIL, ExceptionMessageEnum.CRM_COMMERCIAL_OPPORTUNITY_ADD_FAILED);
            }
        }

        // 插入新数据
        List<ErpCrmProductMarketingUpdateRQ> updateRQList = rq.getProductMarketingUpdateRQList();

        if (!CollectionUtils.isEmpty(updateRQList)) {
            List<ErpCrmProductMarketing> p = BeanUtils.assemble(ErpCrmProductMarketing.class, updateRQList);
            p.forEach(o -> o.setCommercialId(m.getId()).setCreateTime(time).setCreateUser(userId).setDeleted(Status.FALSE.getKey()));
            if (!erpCrmProductMarketingService.insertBatch(p)) {
                log.error("保存子表商机信息失败");
                log.error("修改商机信息失败,调用{}类{}方法出错", "ErpCrmCommercialOpportunityServiceImpl", "addCommercialOpportunity()");
                throw new BusinessException(ResCodeEnum.SAVE_FAIL, ExceptionMessageEnum.CRM_COMMERCIAL_OPPORTUNITY_ADD_FAILED);
            }
        }

        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, m.getId());
    }


    @Override
    public Workbook exportSalesRank(Integer companyId) {

        List<ErpCrmSalesRankDTO> salesRank = crmCommercialOpportunityMapper.getExcelSalesRank(companyId);
        ExportParams exportParams = new ExportParams();
        exportParams.setSheetName("销售排行");
        return ExcelExportUtil.exportExcel(exportParams, ErpCrmSalesRankDTO.class, salesRank);
    }

    /**
     * 商机客户列表导出
     */
    @Override
    public Workbook exportBOC(Integer companyId, CommercialOpportunityQueryRQ rq, AuthPlatformUserInfo userInfo) {
        Map<String, Object> paramMap = new HashMap<>();
        // 不是管理员
        if (!userInfo.getManagerType().equals(1)) {
            ArrayList<Object> userIdList = new ArrayList<>();
            Integer userId = userInfo.getId();
            //查询当前登录用户的在  部门和负责人关联表的部门
            List<ErpCrmDepartmentAdmin> erpCrmDepartmentAdminList = erpCrmDepartmentAdminMapper.selectList(new EntityWrapper<>(new ErpCrmDepartmentAdmin()
                    .setUserId(userInfo.getId())
                    .setEnterpriseId(companyId)));
            userIdList.add(userId);
            // 未建立部门管理人的对应关系，则执行查看下级部门权限的逻辑
            if (CollectionUtils.isEmpty(erpCrmDepartmentAdminList)) {
                ResponseResult<Set<Integer>> responseResult = userInfoFeignClient.subordinates(companyId, userId);
                if (ResCodeEnum.SUCCESS.getCode().equals(responseResult.getCode())) {
                    Set<Integer> subordinates = responseResult.getObject();
                    userIdList.addAll(subordinates);
                }

            } else {
                List<Integer> departments = erpCrmDepartmentAdminList.stream().map(o -> o.getDepartmentId()).collect(Collectors.toList());
                ResponseResult<Set<Integer>> responseResult = authPlatformUserEnterpriseFeignClient.findDepartmentIdAndEnterpriseId(departments, companyId);
                if (ResCodeEnum.SUCCESS.getCode().equals(responseResult.getCode())) {
                    userIdList.addAll(responseResult.getObject());
                }
            }
            paramMap.put("userIdList", userIdList);
        }

        paramMap.put("companyId", companyId);

        if (!StringUtils.isEmpty(rq.getCustomerName())) {
            paramMap.put("customerName", rq.getCustomerName());
        }
        if (!StringUtils.isEmpty(rq.getCustomerType())) {
            paramMap.put("customerType", rq.getCustomerType());
        }
        if (!StringUtils.isEmpty(rq.getPhase())) {
            paramMap.put("phase", rq.getPhase());
        }
        if (!StringUtils.isEmpty(rq.getSaleUserName())) {
            paramMap.put("saleUserName", rq.getSaleUserName());
        }

        List<ErpCrmCommercialBOCDTO> bocList = crmCommercialOpportunityMapper.getexportBOC(paramMap);
        for (ErpCrmCommercialBOCDTO erpCrmCommercialBOCDTO : bocList) {
            ErpCode selectOne = erpCodeMapper.selectOne(new ErpCode().setValue(erpCrmCommercialBOCDTO.getCustomerType()));
            if (Objects.isNull(selectOne)) {
                log.error("没有查到码表客户类型码表不存在", "selectOne");
                erpCrmCommercialBOCDTO.setCustomerType(null);
            } else {
                erpCrmCommercialBOCDTO.setCustomerType(selectOne.getName());
            }
            ErpCode erpCode = erpCodeMapper.selectOne(new ErpCode().setValue(erpCrmCommercialBOCDTO.getPhase()));
            if (Objects.isNull(erpCode)) {
                log.error("没有查到码表客户当前状态码表不存在", "erpCode");
                erpCrmCommercialBOCDTO.setPhase(null);
            } else {
                erpCrmCommercialBOCDTO.setPhase(erpCode.getName());
            }
        }
        ExportParams exportParams = new ExportParams();
        exportParams.setSheetName("商机客户");
        return ExcelExportUtil.exportExcel(exportParams, ErpCrmCommercialBOCDTO.class, bocList);

    }

    private String getAddressById(Integer id) {

        return null;
    }

    /**
     * 删除商机客户
     */
    @Override
	public ResponseResult deleted(AuthPlatformUserInfo userInfo,ErpCrmCommercialOpportunityDeleteRQ deleteRQ){
    	Integer id = userInfo.getId();
    	ErpCrmCommercialOpportunity erpCrmCommercialOpportunity = crmCommercialOpportunityMapper.selectById(deleteRQ.getId()).setDeleted(Status.FALSE.getKey());
    	if (!Objects.isNull(erpCrmCommercialOpportunity) && id .equals(deleteRQ.getSaleUserId()) ) {
    		ErpCode erpCode = erpCodeMapper.selectOne(new ErpCode().setValue(erpCrmCommercialOpportunity.getPhase()));
    		//判断用户是否是成交客户
    		if (!erpCode.getName().equals(EnumErpCode.TRANSACTION_CLIENTS.getValue())) {
    			erpCrmCommercialOpportunity.setDeleted(deleteRQ.getDeleted());
        		crmCommercialOpportunityMapper.updateById(erpCrmCommercialOpportunity);
			}
            return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS);
		}
        return ResponseResult.buildResponseResult(ResCodeEnum.FAILED);
    }

}
