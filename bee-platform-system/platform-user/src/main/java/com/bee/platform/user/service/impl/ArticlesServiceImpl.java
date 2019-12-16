package com.bee.platform.user.service.impl;

import com.baomidou.mybatisplus.mapper.EntityWrapper;
import com.baomidou.mybatisplus.mapper.Wrapper;
import com.baomidou.mybatisplus.plugins.pagination.Pagination;
import com.baomidou.mybatisplus.service.impl.ServiceImpl;
import com.bee.platform.business.dto.ArticleTypesDTO;
import com.bee.platform.business.dto.ArticlesDTO;
import com.bee.platform.business.rq.ArticleAddRQ;
import com.bee.platform.business.rq.ArticleSearchRQ;
import com.bee.platform.common.constants.enums.EnumCommon;
import com.bee.platform.common.entity.AuthPlatformUserInfo;
import com.bee.platform.common.entity.Page;
import com.bee.platform.common.entity.ResCodeEnum;
import com.bee.platform.common.entity.ResponseResult;
import com.bee.platform.common.enums.Status;
import com.bee.platform.common.exception.BusinessException;
import com.bee.platform.common.exception.ExceptionMessageEnum;
import com.bee.platform.common.utils.*;
import com.bee.platform.user.authority.entity.AuthPlatformUser;
import com.bee.platform.user.authority.service.AuthPlatformUserService;
import com.bee.platform.user.dao.mapper.ArticlesMapper;
import com.bee.platform.user.dao.mapper.ArticlesTypeMapper;
import com.bee.platform.user.entity.Articles;
import com.bee.platform.user.entity.ArticlesType;
import com.bee.platform.user.service.ArticlesService;
import com.bee.platform.user.service.EnterprisesUsersService;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.util.CollectionUtils;
import org.springframework.util.ObjectUtils;
import org.springframework.util.StringUtils;

import java.util.*;
import java.util.stream.Collectors;

/**
 * <p>
 *  服务实现类
 * </p>
 *
 * @author jie.zhang123
 * @since 2019-03-04
 */
@Service
public class ArticlesServiceImpl extends ServiceImpl<ArticlesMapper, Articles> implements ArticlesService {

    @Autowired
    private ArticlesMapper articlesMapper;
    @Autowired
    private EnterprisesUsersService enterprisesUsersService;
    @Autowired
    private ArticlesTypeMapper articlesTypeMapper;
    @Autowired
    private AuthPlatformUserService userService;

    @Override
    public ResponseResult< Map<String,Object>> getLastedArticles(AuthPlatformUserInfo userInfo, Page page) {
        Pagination pagination= PageUtils.transFromPage(page);
        //当前用户组织所有文章倒序获取
        List<Articles> articles = articlesMapper.selectPage(pagination,new EntityWrapper<Articles>()
                        .eq("org_id", userInfo.getOrgId()).orderBy("update_at",false));
        List<ArticlesDTO> dtos = BeanUtils.assemble(ArticlesDTO.class, articles);
        if(CollectionUtils.isEmpty(dtos)){
            return ResponseResult.buildResponseResult(ResCodeEnum.ERROR_NOT_FOUND);
        }
        //获取企业用户相关信息 组装参数返回
        dtos.forEach(article -> article.setUsername(
                enterprisesUsersService.getEnterpriseUserInfoById(article.getUserId(), article.getOrgId()).getObject().getNickname()));

        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, CommonUtils.convertToData(dtos,pagination));
    }

    @Override
    public ResponseResult<List<ArticlesDTO>> getAllByAdmin(AuthPlatformUserInfo userInfo,Integer type,Page page) {
        Pagination pagination= PageUtils.transFromPage(page);
        Wrapper<Articles> entityWrapper=new EntityWrapper<Articles>()
                .eq("org_id", userInfo.getOrgId()).orderBy("update_at",false);
        if(!ConstInfos.ARTICLE_TYPE.All.getKey().equals(type)){
            entityWrapper.eq("type", type);
        }
        List<Articles> articles = articlesMapper.selectPage(pagination,entityWrapper);
        List<ArticlesDTO> dtos = BeanUtils.assemble(ArticlesDTO.class, articles);
        if(CollectionUtils.isEmpty(dtos)){
            return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS,new ArrayList<>(),PageUtils.transToPage(pagination));
        }
        //获得最新五条数据
        List<Integer> articlesIds=articlesMapper.getFiveNewest(userInfo.getOrgId());
        //获取企业用户相关信息 组装参数返回
        dtos.forEach(article ->{
            if(articlesIds.contains(article.getId())){
                article.setNewest(Status.TRUE.getKey());
            }else {
                article.setNewest(Status.FALSE.getKey());
            }
            article.setUsername(
                    enterprisesUsersService.getEnterpriseUserInfoById(article.getUserId(), article.getOrgId()).getObject().getNickname());
        });
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS,dtos,PageUtils.transToPage(pagination));
    }

//    @Override
//    public ResponseResult<List<ArticlesDTO>> getArticleList(UserInfo userInfo, ArticleSearchRQ rq,Page page) {
//        Pagination pagination = PageUtils.transFromPage(page);
//        Map<String,Object> map = new HashMap<>(6);
//        if(!StringUtils.isEmpty(rq.getStartTime()) && DateUtils.isValidDate(rq.getStartTime())){
//            map.put("startTime",rq.getStartTime() + " 00:00:00");
//        }
//        if(!StringUtils.isEmpty(rq.getEndTime()) && DateUtils.isValidDate(rq.getEndTime())){
//            map.put("endTime",rq.getEndTime() + " 23:59:59");
//        }
//        if(!StringUtils.isEmpty(rq.getTitle())){
//            map.put("title",rq.getTitle());
//        }
//        if(!ObjectUtils.isEmpty(rq.getType())){
//            map.put("type",rq.getType());
//        }
//        if(!ObjectUtils.isEmpty(userInfo.getOrgId())){
//            map.put("orgId",userInfo.getOrgId());
//        }
//        List<Articles> articles = articlesMapper.getArticles(map,pagination);
//        if(CollectionUtils.isEmpty(articles)){
//            return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS);
//        }
//        List<ArticlesDTO> dtos = BeanUtils.assemble(ArticlesDTO.class, articles);
//        //最新五条数据
//        List<Integer> articlesIds = articlesMapper.getFiveNewest(userInfo.getOrgId());
//        //获取企业用户相关信息 组装参数返回
//        dtos.forEach(article ->{
//            if(articlesIds.contains(article.getId())){
//                article.setNewest(Status.TRUE.getKey());
//            }else {
//                article.setNewest(Status.FALSE.getKey());
//            }
//        });
//        //获取企业用户相关信息 组装参数返回
//       /* dtos.forEach(article -> article.setUsername(
//                enterprisesUsersService.getEnterpriseUserInfoById(article.getUserId(), article.getOrgId()).getObject().getNickname()));*/
//        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS,dtos,PageUtils.transToPage(pagination));
//    }

    /**
     * 条件查询公告列表
     * @param userInfo 用户信息
     * @param rq 请求参数
     * @param page 分页对象
     * @return 公告列表
     */
    @Override
    public ResponseResult<List<ArticlesDTO>> getArticlesByCondition(AuthPlatformUserInfo userInfo, ArticleSearchRQ rq,Page page) {
        Pagination pagination = PageUtils.transFromPage(page);
        Map<String,Object> map = new HashMap<>(6);
        if(!StringUtils.isEmpty(rq.getStartTime()) && DateUtils.isValidDate(rq.getStartTime())){
            map.put("startTime",rq.getStartTime() + " 00:00:00");
        }
        if(!StringUtils.isEmpty(rq.getEndTime()) && DateUtils.isValidDate(rq.getEndTime())){
            map.put("endTime",rq.getEndTime() + " 23:59:59");
        }
        if(!StringUtils.isEmpty(rq.getTitle())){
            map.put("title",rq.getTitle());
        }
        if(!ObjectUtils.isEmpty(rq.getType())){
            map.put("type",rq.getType());
        }
        if(!ObjectUtils.isEmpty(userInfo.getOrgId())){
            map.put("orgId",userInfo.getOrgId());
        }
        List<ArticlesDTO> dto = articlesMapper.getArticlesByCondition(map, pagination);

        //最新五条数据
        List<Integer> articlesIds = articlesMapper.getFiveNewest(userInfo.getOrgId());
        //获取企业用户相关信息 组装参数返回
        dto.forEach(article ->{
            if (StringUtils.isEmpty(article.getUsername()) && !StringUtils.isEmpty(article.getNickname())) {
                article.setUsername(article.getNickname());
            }
            if(articlesIds.contains(article.getId())){
                article.setNewest(Status.TRUE.getKey());
            }else {
                article.setNewest(Status.FALSE.getKey());
            }
        });

         return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS,dto,PageUtils.transToPage(pagination));
    }



    @Override
    public ResponseResult<ArticlesDTO> getArticle(AuthPlatformUserInfo userInfo,int id) {
        Articles article = articlesMapper.selectOne(new Articles().setId(id).setOrgId(userInfo.getOrgId()));
        if (!ObjectUtils.isEmpty(article) && userInfo.getOrgId().equals(article.getOrgId())) {
            //点击量+1
            if(article.getHits()==null){
                article.setHits(1);
            }else {
                article.setHits(article.getHits()+1);
            }
            if(articlesMapper.updateById(article)<=0){
                return ResponseResult.buildResponseResult(ResCodeEnum.UPDATE_ARTICLE_FAILED);
            }
            //组装参数 返回
            Integer userId = article.getUserId();
            ArticlesDTO dto = BeanUtils.copyProperties(article, ArticlesDTO.class);
            AuthPlatformUser user = userService.selectOne(new EntityWrapper<AuthPlatformUser>()
                    .eq("id", userId)
                    .and()
                    .eq("deleted", Status.FALSE.getKey()));
            if(!ObjectUtils.isEmpty(user)){
                String userName = user.getName();
                if (StringUtils.isEmpty(userName) && !StringUtils.isEmpty(user.getNickname())) {
                    userName = user.getNickname();
                }
                dto.setUsername(userName);
            }
            return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS,dto);
        }
        return ResponseResult.buildResponseResult(ResCodeEnum.ARTICLE_NOT_TO_THIS);
    }

    @Override
    @Transactional(rollbackFor = Exception.class)
    public ResponseResult deleteArticle(AuthPlatformUserInfo userInfo,int id) {
        if(ObjectUtils.isEmpty(userInfo.getOrgId())){
            return ResponseResult.buildResponseResult(ResCodeEnum.NO_AUTHORITY);
        }
        Articles article = articlesMapper.selectOne(
                new Articles().setId(id).setOrgId(userInfo.getOrgId()));
        if(ObjectUtils.isEmpty(article)){
            return ResponseResult.buildResponseResult(ResCodeEnum.ERROR_NOT_FOUND);
        }
        if (userInfo.getOrgId().equals(article.getOrgId())) {
            if(articlesMapper.deleteById(article.getId())<=0){
                return ResponseResult.buildResponseResult(ResCodeEnum.DELETE_ARTICLE_FAILED);
            }
            return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS);
        }
        return ResponseResult.buildResponseResult(ResCodeEnum.ERROR_SYSTEM);
    }

    @Override
    @Transactional(rollbackFor = Exception.class)
    public ResponseResult modifyArticle(AuthPlatformUserInfo userInfo, int id, String title, String content, Integer type, String depName, String attachmentName, String attachmentUrl) {
        Articles article = articlesMapper.selectById(id);
        if(ObjectUtils.isEmpty(article)){
            return ResponseResult.buildResponseResult(ResCodeEnum.ERROR_NOT_FOUND);
        }
        if (userInfo.getOrgId().equals(article.getOrgId())) {
            article.setUserId(userInfo.getId());
            article.setTitle(title);
            article.setContent(content);
            //参数组装
            article.setAttachmentName(attachmentName);
            article.setAttachmentUrl(attachmentUrl);
            article.setDepName(depName);
            article.setUpdateAt(new Date());
            //获取所有公告类型
            List<ArticlesType> typeList = articlesTypeMapper.selectList(new EntityWrapper<ArticlesType>()
                    .eq("status", EnumCommon.IsActive.is_active.getKey()));
            if(CollectionUtils.isEmpty(typeList)){
                return ResponseResult.buildResponseResult(ResCodeEnum.GET_ARTICLE_TYPE_FAILED);
            }
            List<Integer> types = typeList.stream().map(e -> e.getId()).collect(Collectors.toList());
            //int[] types = {0,1,2,3}; ArrayUtils.contains(types, type)
            if (types.contains(type)) {
                article.setType(type);
                if(articlesMapper.updateById(article)<=0){
                    return ResponseResult.buildResponseResult(ResCodeEnum.UPDATE_ARTICLE_FAILED);
                }
                return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS);
            }else{
                return ResponseResult.buildResponseResult(ResCodeEnum.ARTICLE_TYPE_ERROR);
            }
        }
        return ResponseResult.buildResponseResult(ResCodeEnum.ARTICLE_NOT_TO_THIS);
    }
    /**
     * @notes 公告编辑
     **/
    @Override
    @Transactional(rollbackFor = Exception.class)
    public ResponseResult<ResCodeEnum> editArticle(AuthPlatformUserInfo userInfo, ArticleAddRQ rq) {
        //公告类型验证
        List<ArticlesType> typeList = articlesTypeMapper.selectList(new EntityWrapper<ArticlesType>()
                .eq("status", EnumCommon.IsActive.is_active.getKey()));
        if(CollectionUtils.isEmpty(typeList)){
            return ResponseResult.buildResponseResult(ResCodeEnum.ARTICLE_TYPE_ERROR);
        }
        List<Integer> types = typeList.stream().map(e -> e.getId()).collect(Collectors.toList());
        if(!types.contains(rq.getType())){
            return ResponseResult.buildResponseResult(ResCodeEnum.ARTICLE_TYPE_ERROR);
        }
        if(ObjectUtils.isEmpty(rq.getId())){
            //新增
            Articles article = new Articles();
            article.setTitle(rq.getTitle());
            article.setContent(rq.getContent());
            article.setAttachmentName(rq.getAttachmentName());
            article.setAttachmentUrl(rq.getAttachmentUrl());
            article.setOrgId(userInfo.getOrgId());
            article.setUserId(userInfo.getId());
            //article.setDepName(rq.getd);
            article.setCreateAt(new Date());
            article.setUpdateAt(new Date());
            article.setType(rq.getType());
            if(articlesMapper.insert(article)<=0){
                return ResponseResult.buildResponseResult(ResCodeEnum.INSERT_ARTICLE_FAILED);
            }
            return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS);
        }
        //修改
        Articles article = articlesMapper.selectOne(new Articles().setId(rq.getId()));
        if(ObjectUtils.isEmpty(article)){
            throw new BusinessException(ResCodeEnum.ARTICLE_NOT_FOUND,ExceptionMessageEnum.ARTICLE_NOT_FOUND);
        }
        article.setTitle(rq.getTitle());
        article.setContent(rq.getContent());
        article.setAttachmentName(rq.getAttachmentName());
        article.setAttachmentUrl(rq.getAttachmentUrl());
        article.setOrgId(userInfo.getOrgId());
        article.setUserId(userInfo.getId());
        //article.setDepName(rq.getd);
        article.setCreateAt(new Date());
        article.setUpdateAt(new Date());
        article.setType(rq.getType());
        if(articlesMapper.updateById(article)<=0){
            return ResponseResult.buildResponseResult(ResCodeEnum.UPDATE_ARTICLE_FAILED);
        }
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS);
    }

    @Override
    public ResponseResult<List<ArticleTypesDTO>> getArticleTypes(AuthPlatformUserInfo userInfo, String name, Page page) {
        Pagination pagination = PageUtils.transFromPage(page);
        Wrapper<ArticlesType> entityWrapper = new EntityWrapper<ArticlesType>()
                .eq("org_id",userInfo.getOrgId())
                .eq("status",EnumCommon.IsActive.is_active.getKey())
                .like("name",name);
        List<ArticlesType> types = articlesTypeMapper.selectPage(pagination,entityWrapper);
        if(CollectionUtils.isEmpty(types)){
            return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS,new ArrayList<>(),PageUtils.transToPage(pagination));
        }
        List<ArticleTypesDTO> result = BeanUtils.assemble(ArticleTypesDTO.class, types);
        Integer orgId = userInfo.getOrgId();
        for (ArticleTypesDTO type:result) {
            //公告对应文章数
            Integer typeId = type.getId();
            Integer count = articlesMapper.selectCount(new EntityWrapper<Articles>()
                    .eq("type", typeId)
                    .eq("org_id",orgId));
            type.setCount(count);
        }
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS,result,PageUtils.transToPage(pagination));
    }

    @Override
    @Transactional(rollbackFor = Exception.class)
    public ResponseResult deleteArticleType(AuthPlatformUserInfo userInfo,int id) {
        //该类型下是否有公告
        Integer count = articlesMapper.selectCount(new EntityWrapper<Articles>().eq("type", id));
        if(count>0){
            throw new BusinessException(ResCodeEnum.ARTICLE_TYPE_NOT_EMPTY,ExceptionMessageEnum.ARTICLE_TYPE_NOT_EMPTY);
        }
        ArticlesType articlesType = articlesTypeMapper.selectById(id);
        if(ObjectUtils.isEmpty(articlesType)){
            return ResponseResult.buildResponseResult(ResCodeEnum.ERROR_NOT_FOUND);
        }
        articlesType.setStatus(EnumCommon.IsActive.not_active.getKey());
        if(articlesTypeMapper.updateById(articlesType)<=0){
            return ResponseResult.buildResponseResult(ResCodeEnum.DELETE_ARTICLE_FAILED);
        }
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS);
    }

    @Override
    @Transactional(rollbackFor = Exception.class)
    public ResponseResult modifyArticleType(AuthPlatformUserInfo userInfo, Integer id, String name) {
        //新增
        if(ObjectUtils.isEmpty(id)){
            if(articlesTypeMapper.insert(
                    new ArticlesType().setName(name).setStatus(EnumCommon.IsActive.is_active.getKey()).setOrgId(userInfo.getOrgId()))<=0){
                return ResponseResult.buildResponseResult(ResCodeEnum.UPDATE_ARTICLE_TYPE_FAILED);
            }
            return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS);
        }
        //编辑
        ArticlesType articlesType = articlesTypeMapper.selectById(id);
        if(ObjectUtils.isEmpty(articlesType)){
            return ResponseResult.buildResponseResult(ResCodeEnum.ERROR_NOT_FOUND);
        }
        articlesType.setName(name);
        if(articlesTypeMapper.updateById(articlesType)<=0){
            return ResponseResult.buildResponseResult(ResCodeEnum.UPDATE_ARTICLE_TYPE_FAILED);
        }
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS);
    }
}
