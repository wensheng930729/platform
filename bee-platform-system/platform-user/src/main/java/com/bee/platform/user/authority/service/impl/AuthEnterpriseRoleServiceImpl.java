package com.bee.platform.user.authority.service.impl;

import com.baomidou.mybatisplus.service.impl.ServiceImpl;
import com.bee.platform.common.entity.ResCodeEnum;
import com.bee.platform.common.entity.ResponseResult;
import com.bee.platform.common.enums.EnumRoleType;
import com.bee.platform.common.service.JedisService;
import com.bee.platform.common.utils.ConstantsUtil;
import com.bee.platform.user.authority.dao.mapper.AuthEnterpriseRoleMapper;
import com.bee.platform.user.authority.dto.AuthEnterpriseRoleTreeDTO;
import com.bee.platform.user.authority.dto.EnterpriseUrlDTO;
import com.bee.platform.user.authority.entity.AuthEnterpriseRole;
import com.bee.platform.user.authority.service.AuthEnterpriseRoleService;
import com.google.common.collect.Lists;
import com.google.common.collect.Sets;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.util.CollectionUtils;
import org.springframework.util.ObjectUtils;

import java.util.ArrayList;
import java.util.List;
import java.util.Set;
import java.util.stream.Collectors;

/**
 * <p>
 * 企业与角色（角色或功能的关联表）的中间表 服务实现类
 * </p>
 *
 * @author LILIANG
 * @since 2019-05-20
 */
@Slf4j
@Service
public class AuthEnterpriseRoleServiceImpl extends ServiceImpl<AuthEnterpriseRoleMapper, AuthEnterpriseRole> implements AuthEnterpriseRoleService {

    @Autowired
    private AuthEnterpriseRoleMapper enterpriseRoleMapper;


    @Autowired
    private AuthEnterpriseRoleMapper authEnterpriseRoleMapper;

    @Autowired
    private JedisService jedisService;

    /**
     * 查询企业下所有应用树
     *
     * @param enterpriseId 企业id
     * @return 企业下所有应用树
     */
    @Override
    public ResponseResult<List<AuthEnterpriseRoleTreeDTO>> getEnterpriseRoleTreeList(Integer enterpriseId) {

        // 根据企业id查询企业拥有的应用、功能一、功能二
        List<AuthEnterpriseRoleTreeDTO> treeOne = authEnterpriseRoleMapper.getEnterpriseRole(enterpriseId);
        // 获取app的ids
        List<Integer> pIds = treeOne.stream().filter(o -> EnumRoleType.FUNCTION_ONE.getCode().equals(o.getRoleType())).map(AuthEnterpriseRoleTreeDTO::getPid).collect(Collectors.toList());
        List<AuthEnterpriseRoleTreeDTO> app = null;
        if (!CollectionUtils.isEmpty(pIds)) {
            app = authEnterpriseRoleMapper.getEnterpriseAppRole(pIds);
        }
        List<Integer> ids = treeOne.stream().filter(o -> EnumRoleType.FUNCTION_TWO.getCode().equals(o.getRoleType())).map(AuthEnterpriseRoleTreeDTO::getId).collect(Collectors.toList());
        List<AuthEnterpriseRoleTreeDTO> treeTwo = null;
        // 根据二级功能ids查询企业拥有的基础角色
        if (!CollectionUtils.isEmpty(ids)) {
            treeTwo = authEnterpriseRoleMapper.getEnterpriseBaseRole(ids);
        }

        // 获取企业下的base记录集合
        Set<AuthEnterpriseRoleTreeDTO> total = Sets.newHashSet();
        if(!CollectionUtils.isEmpty(app)){
            total.addAll(app);
        }
        if (!CollectionUtils.isEmpty(treeOne)) {
            total.addAll(treeOne);
        }
        if (!CollectionUtils.isEmpty(treeTwo)) {
            total.addAll(treeTwo);
        }

        List<AuthEnterpriseRoleTreeDTO> treeDTOS = build(total);

        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, treeDTOS);
    }

    /**
     * @param enterpriseRoles :
     * @notes: 批量插入企业角色的关联数据
     * @Author: junyang.li
     * @Date: 16:01 2019/5/24
     * @return: void
     */
    @Override
    @Transactional(rollbackFor = Exception.class)
    public void insertAll(List<AuthEnterpriseRole> enterpriseRoles) {
        if (!CollectionUtils.isEmpty(enterpriseRoles)) {
            enterpriseRoleMapper.insertAll(enterpriseRoles);
        }
    }


    /**
     * 两层循环实现建树
     *
     * @param treeNodes 传入的树节点列表
     * @return List<AuthEnterpriseRoleTreeDTO>
     */
    private static List<AuthEnterpriseRoleTreeDTO> build(Set<AuthEnterpriseRoleTreeDTO> treeNodes) {

        List<AuthEnterpriseRoleTreeDTO> trees = new ArrayList<>();

        for (AuthEnterpriseRoleTreeDTO treeNode : treeNodes) {

            if (!ObjectUtils.isEmpty(treeNode.getPid()) && 0 == (treeNode.getPid())) {
                trees.add(treeNode);
            }

            for (AuthEnterpriseRoleTreeDTO it : treeNodes) {
                if (treeNode.getId().equals(it.getPid())) {
                    if (treeNode.getChildren() == null) {
                        treeNode.setChildren(new ArrayList<>());
                    }
                    treeNode.getChildren().add(it);
                }
            }
        }
        return trees;
    }

    /**
     * 获取企业下的接口url
     *
     * @param enterpriseId 企业id
     * @param subSys       系统标识
     * @return 企业下的url
     */
    @Override
    public List<EnterpriseUrlDTO> getEnterpriseInterfaceUri(Integer enterpriseId, String subSys) {

        try {
            String key = ConstantsUtil.COMMON_AUTH_ENTERPRISE_URL + enterpriseId;

            List<EnterpriseUrlDTO> dto;
            // 先从缓存查
            dto = jedisService.getJsonArrayObject(key, EnterpriseUrlDTO.class);
            if (ObjectUtils.isEmpty(dto)) {
                // 从数据库查询
                dto = authEnterpriseRoleMapper.getEnterpriseInterfaceUri(enterpriseId, subSys);
                jedisService.delKey(key);
                jedisService.setJsonObject(key, dto, 0);
            }
            return dto;
        } catch (Exception e) {
            log.error("缓存连接异常，查询的键是：{}，异常信息是：{}", ConstantsUtil.COMMON_AUTH_ENTERPRISE_URL + enterpriseId, e);
            return authEnterpriseRoleMapper.getEnterpriseInterfaceUri(enterpriseId, subSys);
        }
    }


}
