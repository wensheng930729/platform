package com.bee.platform.user.service.impl;

import com.baomidou.mybatisplus.mapper.EntityWrapper;
import com.baomidou.mybatisplus.service.impl.ServiceImpl;
import com.bee.platform.common.constants.enums.EnumCommon;
import com.bee.platform.common.entity.AuthPlatformUserInfo;
import com.bee.platform.common.entity.ResCodeEnum;
import com.bee.platform.common.entity.ResponseResult;
import com.bee.platform.common.entity.UserInfo;
import com.bee.platform.common.utils.BeanUtils;
import com.bee.platform.user.constants.enums.EnumEnterpriseCheck;
import com.bee.platform.user.dao.mapper.ArticlesTypeMapper;
import com.bee.platform.user.dto.ArticlesTypeDTO;
import com.bee.platform.user.entity.ArticlesType;
import com.bee.platform.user.service.ArticlesTypeService;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.util.CollectionUtils;

import java.util.List;

/**
 * <p>
 *  服务实现类
 * </p>
 *
 * @author jie.zhang123
 * @since 2019-04-24
 */
@Slf4j
@Service
public class ArticlesTypeServiceImpl extends ServiceImpl<ArticlesTypeMapper, ArticlesType> implements ArticlesTypeService {

    @Autowired
    private ArticlesTypeMapper typeMapper;

    /**
     * 查询所有公告类型列表
     * @return 公告类型列表
     */
    @Override
    public List<ArticlesTypeDTO> getAllArticlesType(AuthPlatformUserInfo userInfo) {
        log.info("查询所有公告类型");
        List<ArticlesType> articlesTypeList = this.selectList(new EntityWrapper<ArticlesType>()
                .eq("status", EnumCommon.LogicStatus.NORMAL.getKey())
                .eq("org_id",userInfo.getOrgId()));
        return BeanUtils.assemble(ArticlesTypeDTO.class, articlesTypeList);
    }

    @Override
    public ResponseResult<Integer> checkTypeUnique(AuthPlatformUserInfo userInfo, String name) {
        List<ArticlesType> types = typeMapper.selectList(new EntityWrapper<ArticlesType>()
                .eq("name", name)
                .eq("status", EnumCommon.LogicStatus.NORMAL.getKey())
                .eq("org_id",userInfo.getOrgId()));
        if(CollectionUtils.isEmpty(types)){
            return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, EnumEnterpriseCheck.IS_EXISTED.NO.getKey());
        }
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, EnumEnterpriseCheck.IS_EXISTED.YES.getKey());
    }
}
