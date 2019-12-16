package com.bee.platform.datadriver.service.impl;

import com.baomidou.mybatisplus.mapper.EntityWrapper;
import com.baomidou.mybatisplus.mapper.Wrapper;
import com.baomidou.mybatisplus.plugins.pagination.Pagination;
import com.baomidou.mybatisplus.service.impl.ServiceImpl;
import com.bee.platform.common.constants.enums.EnumCommon;
import com.bee.platform.common.entity.AuthPlatformUserInfo;
import com.bee.platform.common.entity.ResCodeEnum;
import com.bee.platform.common.entity.ResponseResult;
import com.bee.platform.common.enums.Status;
import com.bee.platform.common.utils.BeanUtils;
import com.bee.platform.common.utils.PageUtils;
import com.bee.platform.datadriver.dao.mapper.*;
import com.bee.platform.datadriver.dto.ErpRepositoryBoxDTO;
import com.bee.platform.datadriver.dto.ErpRepositoryDetailsDTO;
import com.bee.platform.datadriver.dto.ErpRepositoryListDTO;
import com.bee.platform.datadriver.entity.*;
import com.bee.platform.datadriver.rq.ErpRepositoryAddRQ;
import com.bee.platform.datadriver.rq.ErpRepositoryDeleteRQ;
import com.bee.platform.datadriver.rq.ErpRepositoryEnableRQ;
import com.bee.platform.datadriver.rq.ErpRepositoryUpdataRQ;
import com.bee.platform.datadriver.service.ErpRepositoryService;
import com.bee.platform.user.authority.dto.AuthEnterpriseDTO;
import com.bee.platform.user.authority.dto.AuthEnterpriseFeignDetailDTO;
import com.bee.platform.user.service.feign.AuthEnterpriseFeignClient;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.util.CollectionUtils;

import java.util.ArrayList;
import java.util.Date;
import java.util.List;
import java.util.Objects;

/**
 * <p>
 * 仓库档案 服务实现类
 * </p>
 *
 * @author hongchuan.he
 * @since 2019-05-27
 */
@Slf4j
@Service
public class ErpRepositoryServiceImpl extends ServiceImpl<ErpRepositoryMapper, ErpRepository> implements ErpRepositoryService {

    @Autowired
    private ErpRepositoryMapper erpRepositoryMapper;
    @Autowired
    private AuthEnterpriseFeignClient authEnterpriseFeignClient;
    @Autowired
    private ErpStockCheckDetailMapper erpStockCheckDetailMapper;
    @Autowired
    private ErpRepoReceiptDetailMapper erpRepoReceiptDetailMapper;
    @Autowired
    private ErpOutOfStockOrderDetailMapper erpOutOfStockOrderDetailMapper;
    @Autowired
    private ErpWarehousingOrderMapper erpWarehousingOrderMapper;
    @Autowired
    private ErpOpeningInventoryOrderDetailMapper erpOpeningInventoryOrderDetailMapper;
    private static Integer ZERO = 0;

    /**
     * 查询仓库档案
     */
    @Override
    public ResponseResult<List<ErpRepositoryListDTO>> query(Pagination pagination, Integer orgId, Integer status) {
//		List<AuthEnterpriseFlatDTO> enterpriseFlatDTOS = authEnterpriseFeignClient.getEnterpriseFlatByCompanyId(companyId).getObject();
//		if (org.springframework.util.CollectionUtils.isEmpty(enterpriseFlatDTOS)) {
//			log.error("未查询到当前用户企业信息！ 类：{} 方法：{}", "ErpRepositoryServiceImpl", "query");
//			return ResponseResult.buildResponseResult(ResCodeEnum.GET_ENTERPRISE_USER_RELATION_FAILED, Lists.newArrayList());
//		}
//		List<Integer> enterpriseIds = enterpriseFlatDTOS.stream().map(AuthEnterpriseFlatDTO::getValue).collect(Collectors.toList());
//		if (CollectionUtils.isEmpty(enterpriseIds)) {
//			return ResponseResult.buildResponseResult(ResCodeEnum.ERROR_NOT_FOUND);
//		}
        // 查询公司详情
        ArrayList<Integer> orgIds = new ArrayList<>();
        orgIds.add(orgId);
        List<AuthEnterpriseFeignDetailDTO> detailList = authEnterpriseFeignClient.getEnterpriseMoreDetail(orgIds).getObject();
        String companyName = null;
        if (!CollectionUtils.isEmpty(detailList)) {
            companyName = detailList.get(0).getName();
        }
        List<ErpRepositoryListDTO> resultList = erpRepositoryMapper.query(pagination, orgId, status);
        if (CollectionUtils.isEmpty(resultList)) {
            return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, new ArrayList<>(0), PageUtils.transToPage(pagination));
        }
        if (null != companyName) {
            for (ErpRepositoryListDTO dto : resultList) {
                dto.setCompanyName(companyName);
            }
        }
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, resultList, PageUtils.transToPage(pagination));
    }


    /**
     * 仓库档案启用还是禁用
     */
    @Override
    @Transactional(rollbackFor = Exception.class)
    public ResponseResult<Integer> updateErpRepositoryEnable(AuthPlatformUserInfo userInfo, ErpRepositoryEnableRQ enableRQ) {
        ErpRepository erpRepository = erpRepositoryMapper.selectOne(new ErpRepository().setId(enableRQ.getId()));
        if (Objects.isNull(erpRepository)) {
            return ResponseResult.buildResponseResult(ResCodeEnum.NO_DATA);
        }
        if (1 == erpRepository.getDeleted()) {
            return ResponseResult.buildResponseResult(ResCodeEnum.NO_DATA);
        }
        erpRepository.setStatus(enableRQ.getStatus());
        erpRepositoryMapper.updateById(erpRepository);
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, erpRepository.getId());

    }


    /**
     * 仓库档案删除
     */
    @Override
    @Transactional(rollbackFor = Exception.class)
    public ResponseResult<Integer> delete(AuthPlatformUserInfo userInfo, ErpRepositoryDeleteRQ deleteRQ) {
        // 校验仓库是否被使用
        if (this.checkErpRepositoryUseInfo(deleteRQ.getId(), userInfo.getOrgId())) {
            return ResponseResult.buildResponseResult(ResCodeEnum.REPOSITORY_IS_USED);
        }
        ErpRepository erpRepository = erpRepositoryMapper.selectOne(new ErpRepository().setId(deleteRQ.getId()).setDeleted(EnumCommon.IsDeleted.not_Delete.getKey()));
        if (Objects.isNull(erpRepository)) {
            return ResponseResult.buildResponseResult(ResCodeEnum.REPOSITORY_NO);
        }
        erpRepository.setDeleted(deleteRQ.getDeleted());
        if (!this.updateById(erpRepository)) {
            return ResponseResult.buildResponseResult(ResCodeEnum.DELETE_FAIL);
        }
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, erpRepository.getId());

    }

    /**
     * 校验仓库是否被使用
     *
     * @param id
     * @param orgId
     * @return
     */
    private boolean checkErpRepositoryUseInfo(Integer id, Integer orgId) {
        //库存盘点明细
        Integer erpStockCheckDetailCount = erpStockCheckDetailMapper.selectCount(new EntityWrapper<>(new ErpStockCheckDetail()
                .setDeleted(Status.FALSE.getKey()).setRepositoryId(id)));
        //原料入库，成品出库单明细
        Integer erpRepoReceiptDetailCount = erpRepoReceiptDetailMapper.selectCount(new EntityWrapper<>(new ErpRepoReceiptDetail()
                .setDeleted(Status.FALSE.getKey()).setRepositoryId(id)));
        //领料出库明细表
        Integer erpOutOfStockOrderDetailCount = erpOutOfStockOrderDetailMapper.selectCount(new EntityWrapper<>(new ErpOutOfStockOrderDetail()
                .setDeleted(Status.FALSE.getKey()).setRepositoryId(id)));
        // 成品入库
        Integer warehousingOrderCount = erpWarehousingOrderMapper.selectCount(new EntityWrapper<>(new ErpWarehousingOrder()
                .setDeleted(Status.FALSE.getKey()).setRepositoryId(id)));
        Integer erpOpeningInventoryOrderDetaiCount = erpOpeningInventoryOrderDetailMapper.selectCount(new EntityWrapper<>(new ErpOpeningInventoryOrderDetail()
                .setDeleted(Status.FALSE.getKey()).setRepositoryId(id)));
        Integer total = erpStockCheckDetailCount + erpRepoReceiptDetailCount + erpOutOfStockOrderDetailCount + warehousingOrderCount + erpOpeningInventoryOrderDetaiCount;
        return total > ZERO;
    }

    /**
     * 添加仓库档案
     */
    @Override
    @Transactional(rollbackFor = Exception.class)
    public ResponseResult<Integer> add(AuthPlatformUserInfo userInfo, ErpRepositoryAddRQ addRQ) {
        ErpRepository erpRepository = new ErpRepository();
        // 校验仓库档案名称是否重复
        List<ErpRepository> furnaceNameCheckList = erpRepositoryMapper.selectList(new EntityWrapper<>(new ErpRepository()
                .setDeleted(EnumCommon.IsDeleted.not_Delete.getKey())
                .setOrgId(addRQ.getOrgId())
                .setName(addRQ.getName())));
        if (!CollectionUtils.isEmpty(furnaceNameCheckList)) {
            return ResponseResult.buildResponseResult(ResCodeEnum.REPOSITORY_NAME_EXIST);
        }
        BeanUtils.copyProperties(addRQ, erpRepository);
        //默认没有删除
        erpRepository.setDeleted(Status.FALSE.getKey())
                .setCreaterId(userInfo.getId())
                .setCreateTime(new Date());
        erpRepositoryMapper.insert(erpRepository);
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, erpRepository.getId());
    }

    /**
     * 编辑需要返回的数据
     */
    @Override
    public ResponseResult get(int id) {
        ErpRepository erpRepository = erpRepositoryMapper.selectOne(new ErpRepository().setId(id));
        if (Objects.isNull(erpRepository)) {
            return ResponseResult.buildResponseResult(ResCodeEnum.NO_DATA);
        }
        if (1 == erpRepository.getDeleted()) {
            return ResponseResult.buildResponseResult(ResCodeEnum.NO_DATA);
        }
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, erpRepository);

    }

    /**
     * 编辑
     */
    @Override
    @Transactional(rollbackFor = Exception.class)
    public ResponseResult<Integer> update(AuthPlatformUserInfo userInfo, ErpRepositoryUpdataRQ rq) {
        ErpRepository erpRepository = erpRepositoryMapper.selectOne(new ErpRepository().setId(rq.getId()).setDeleted(EnumCommon.IsDeleted.not_Delete.getKey()));
        if (Objects.isNull(erpRepository)) {
            return ResponseResult.buildResponseResult(ResCodeEnum.NO_DATA);
        }
        // 校验设备名称是否重复
        if (!rq.getName().equals(erpRepository.getName())) {
            List<ErpRepository> furnaceNameCheckList = this.selectList(new EntityWrapper<ErpRepository>(new ErpRepository()
                    .setDeleted(EnumCommon.IsDeleted.not_Delete.getKey())
                    .setOrgId(rq.getOrgId())
                    .setName(rq.getName())));
            if (!CollectionUtils.isEmpty(furnaceNameCheckList)) {
                return ResponseResult.buildResponseResult(ResCodeEnum.FURNACE_NAME_EXIST);
            }
        }
        BeanUtils.copyProperties(rq, erpRepository);
        if (erpRepositoryMapper.updateById(erpRepository) != 1) {
            return ResponseResult.buildResponseResult(ResCodeEnum.UPDATE_FAIL);
        }
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, erpRepository.getId());
    }

    @Override
    public List<ErpRepositoryBoxDTO> getRepositoryList(AuthPlatformUserInfo userInfo, String sysToken, List<String> categoryList) {
        Integer orgId = userInfo.getOrgId();
//        List<AuthEnterpriseFlatDTO> enterpriseFlatDTOS = authEnterpriseFeignClient.getEnterpriseFlatByCompanyId(orgId).getObject();
//        if (org.springframework.util.CollectionUtils.isEmpty(enterpriseFlatDTOS)) {
//            log.error("未查询到当前用户企业信息！ 类：{} 方法：{}", "ErpRepositoryServiceImpl", "getRepositoryList");
//            return Lists.newArrayList();
//        }
//        List<Integer> ids = enterpriseFlatDTOS.stream().map(a -> a.getValue()).collect(Collectors.toList());
//
//        if (org.springframework.util.CollectionUtils.isEmpty(ids)) {
//            return Lists.newArrayList();
//        }

        Wrapper<ErpRepository> wrapper = new EntityWrapper<>(new ErpRepository()
                .setOrgId(orgId)
                .setStatus(Status.TRUE.getKey())
                .setDeleted(Status.FALSE.getKey()))
                .orderBy("create_time", false);
        if (!CollectionUtils.isEmpty(categoryList) && categoryList.size() > 0) {
            wrapper.in("type", categoryList);
        }
        List<ErpRepository> erpRepositories = selectList(wrapper);
        return BeanUtils.assemble(ErpRepositoryBoxDTO.class, erpRepositories);

    }

    /**
     * 根据状态查询仓库档案
     */
    @Override
    public ResponseResult<List<ErpRepositoryDetailsDTO>> getStatus(Pagination pagination, Integer companyId, Integer status) {
        List<ErpRepository> erpRepositories = null;
//        List<AuthEnterpriseFlatDTO> enterpriseFlatDTOS = authEnterpriseFeignClient.getEnterpriseFlatByCompanyId(companyId).getObject();
//        if (org.springframework.util.CollectionUtils.isEmpty(enterpriseFlatDTOS)) {
//            log.error("未查询到当前用户企业信息！ 类：{} 方法：{}", "ErpRepositoryServiceImpl", "query");
//            return ResponseResult.buildResponseResult(ResCodeEnum.GET_ENTERPRISE_USER_RELATION_FAILED, Lists.newArrayList());
//        }
//        //获取所有的企业id
//        List<Integer> enterpriseIds = enterpriseFlatDTOS.stream().map(AuthEnterpriseFlatDTO::getValue).collect(Collectors.toList());
//        if (CollectionUtils.isEmpty(enterpriseIds)) {
//            return ResponseResult.buildResponseResult(ResCodeEnum.ERROR_NOT_FOUND);
//        }
        if (null == status) {
            erpRepositories = erpRepositoryMapper.selectPage(pagination, new EntityWrapper<>(new ErpRepository()
                    .setOrgId(companyId)
                    .setDeleted(Status.FALSE.getKey()))
                    .orderBy("create_time", false));
        } else {
            //查询当前登录人关联企业的所有仓库档案
            erpRepositories = erpRepositoryMapper.selectPage(pagination, new EntityWrapper<>(new ErpRepository()
                    .setOrgId(companyId)
                    .setDeleted(Status.FALSE.getKey())
                    .setStatus(status))
                    .orderBy("create_time", false));
        }
        if (CollectionUtils.isEmpty(erpRepositories)) {
            return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, new ArrayList<>(0), PageUtils.transToPage(pagination));
        }
        List<ErpRepositoryDetailsDTO> list = BeanUtils.assemble(ErpRepositoryDetailsDTO.class, erpRepositories);
        for (ErpRepositoryDetailsDTO e : list) {
            ResponseResult<AuthEnterpriseDTO> responseResult = authEnterpriseFeignClient.getParentEnterpriseId(e.getOrgId());
            if (responseResult == null) {
                log.info("调用authEnterpriseFeignClient出错");
            }
            e.setCompanyName(responseResult == null ? "" : responseResult.getObject().getName());
        }

        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, list, PageUtils.transToPage(pagination));

    }
}
