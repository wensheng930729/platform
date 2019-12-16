package com.bee.platform.user.service;

import com.baomidou.mybatisplus.service.IService;
import com.bee.platform.business.dto.ArticleTypesDTO;
import com.bee.platform.business.rq.ArticleAddRQ;
import com.bee.platform.business.rq.ArticleSearchRQ;
import com.bee.platform.business.dto.ArticlesDTO;
import com.bee.platform.common.entity.*;
import com.bee.platform.user.entity.Articles;

import java.util.List;
import java.util.Map;

/**
 * <p>
 *  服务类
 * </p>
 *
 * @author jie.zhang123
 * @since 2019-03-04
 */
public interface ArticlesService extends IService<Articles> {

    /**
     * 用户获取文章列表
     * @return
     */
    ResponseResult< Map<String,Object>> getLastedArticles(AuthPlatformUserInfo userInfo, Page page);

    /**
     * 获取文章列表
     * @param type
     * @param
     * @return
     */
    ResponseResult<List<ArticlesDTO>> getAllByAdmin(AuthPlatformUserInfo userInfo,Integer type, Page page);

    /**
     * 按标题搜索文章
     * @param
     * @return
     */
//    ResponseResult<List<ArticlesDTO>> getArticleList(UserInfo userInfo, ArticleSearchRQ rq,Page page);

    /**
     * 根据条件查询公告
     * @param userInfo 用户信息
     * @param rq 请求参数
     * @param page 分页对象
     * @return
     */
    ResponseResult<List<ArticlesDTO>> getArticlesByCondition(AuthPlatformUserInfo userInfo, ArticleSearchRQ rq,Page page);

    /**
     * id获取文章信息
     * @param id
     * @return
     */
    ResponseResult<ArticlesDTO> getArticle(AuthPlatformUserInfo userInfo,int id);

    /**
     * 删除文章
     * @param id
     * @return
     */
    ResponseResult deleteArticle(AuthPlatformUserInfo userInfo,int id);

    /**
     * 修改文章
     * @param userInfo
     * @param id
     * @param title
     * @param content
     * @param type
     * @param depName
     * @param attachmentName
     * @param attachmentUrl
     * @return
     */
    ResponseResult modifyArticle(AuthPlatformUserInfo userInfo, int id, String title, String content, Integer type, String depName, String attachmentName, String attachmentUrl);

    /**
     * 发布文章
     * @param userInfo
     * @return
     */
    ResponseResult<ResCodeEnum> editArticle(AuthPlatformUserInfo userInfo, ArticleAddRQ rq);

    /**
     * 获取公共类型列表
     * @param userInfo
     * @param name
     * @param page
     * @return
     */
    ResponseResult<List<ArticleTypesDTO>> getArticleTypes(AuthPlatformUserInfo userInfo, String name, Page page);

    /**
     * 删除公告类型
     * @param userInfo
     * @param id
     * @return
     */
    ResponseResult deleteArticleType(AuthPlatformUserInfo userInfo, int id);

    /**
     * 新增 编辑公告类型
     * @param userInfo
     * @param id
     * @param name
     * @return
     */
    ResponseResult modifyArticleType(AuthPlatformUserInfo userInfo, Integer id, String name);
}
