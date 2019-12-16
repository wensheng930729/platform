 package com.bee.platform.user.service.impl;

 import com.baomidou.mybatisplus.mapper.Wrapper;
 import com.baomidou.mybatisplus.plugins.pagination.Pagination;
 import com.baomidou.mybatisplus.service.impl.ServiceImpl;
 import com.bee.platform.business.rq.NewsInfoRQ;
 import com.bee.platform.common.entity.ResCodeEnum;
 import com.bee.platform.common.entity.ResponseResult;
 import com.bee.platform.common.utils.BeanUtils;
 import com.bee.platform.common.utils.PageUtils;
 import com.bee.platform.user.dao.mapper.NewsMapper;
 import com.bee.platform.user.dao.mapper.NewsTypeMapper;
 import com.bee.platform.user.dto.NewDTO;
 import com.bee.platform.user.dto.NewDetailDTO;
 import com.bee.platform.user.dto.NewsInfoDTO;
 import com.bee.platform.user.entity.News;
 import com.bee.platform.user.entity.NewsType;
 import com.bee.platform.user.entity.PlatformManagers;
 import com.bee.platform.user.service.NewsService;
 import com.bee.platform.user.service.PlatformManagersService;
 import lombok.extern.slf4j.Slf4j;
 import org.apache.commons.lang3.StringUtils;
 import org.springframework.beans.factory.annotation.Autowired;
 import org.springframework.stereotype.Service;
 import org.springframework.transaction.annotation.Transactional;
 import org.springframework.util.CollectionUtils;
 import org.springframework.util.ObjectUtils;

 import java.io.Serializable;
 import java.util.ArrayList;
 import java.util.Date;
 import java.util.List;

 @Slf4j
@Service
@Transactional(rollbackFor = Exception.class)
public class NewsServiceImpl extends ServiceImpl<NewsMapper, News> implements NewsService {
    
     @Autowired
     private NewsMapper newsMapper;
     @Autowired
     private NewsTypeMapper newsTypeMapper;
     @Autowired
     private PlatformManagersService platformManagersService;

     @Override
     public ResponseResult listByPage(Pagination pagination, Wrapper<News> wrapper) {
         try {
             List<News> news = newsMapper.selectPage(pagination, wrapper);
             return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, news, PageUtils.transToPage(pagination));
         } catch (Exception e) {
             log.error("分页查询资讯出错", e);
         }
         return ResponseResult.buildResponseResult(ResCodeEnum.FAILED);
     }

     @Override
     public ResponseResult<NewDTO> listByPageable(Pagination page, Wrapper<News> wrapper) {
         List<String> desc=new ArrayList<>();
         desc.add("createAt");
         List<News> list = newsMapper.selectPage(page, wrapper.orderDesc(desc));
         if(CollectionUtils.isEmpty(list)){
             return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS);
         }
         List<NewDetailDTO> dtos=BeanUtils.assemble(NewDetailDTO.class,list);
         NewDTO dto=new NewDTO().setContent(dtos).setTotalPages(page.getPages())
                 .setTotalElements(page.getTotal()).setSize(page.getSize())
                 .setNumber(page.getCurrent());
         return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS,dto);
     }

     @Override
    public ResponseResult<Long> countAll() {
        try {
            Long count = newsMapper.countAll();
            return ResponseResult.success(count);
        } catch (Exception e) {
            log.error("获取资讯数量出错", e);
        }
        return ResponseResult.fail(ResCodeEnum.ERROR_SYSTEM);
    }

    @Override
    public ResponseResult<News> hitNews(Serializable id) {
        News news = newsMapper.selectById(id);
        if(news.getHits()==null){
            news.setHits(1);
        }else {
            news.setHits(news.getHits() + 1);
        }
        newsMapper.updateById(news);
        return ResponseResult.success(news);
    }

    @Override
    @Transactional(rollbackFor = Exception.class)
    public ResponseResult addNews(String username, NewsInfoRQ newsInfoRQ) {
        try {
            PlatformManagers platformManagers = platformManagersService.getManagerByName(username);
            News news = new News();
            news.setTitle(newsInfoRQ.getTitle());
            news.setContent(newsInfoRQ.getContent());
            news.setNewsSource(newsInfoRQ.getNew_source());
            news.setImage(newsInfoRQ.getImage());
            news.setType(newsInfoRQ.getType());
            news.setCreateAt(new Date());
            news.setUpdateAt(new Date());
            news.setUserId(platformManagers.getManagerId());
            news.setState(0);
            newsMapper.insert(news);
            log.info("资讯创建成功");
            return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS);
        } catch (Exception e) {
            e.printStackTrace();
            return ResponseResult.buildResponseResult(ResCodeEnum.SAVE_FAIL);
        }
    }

    @Override
    @Transactional(rollbackFor = Exception.class)
    public ResponseResult updateNews(NewsInfoRQ newsInfoRQ) {
        try {
            News news = newsMapper.selectById(newsInfoRQ.getId());
            news.setTitle(newsInfoRQ.getTitle());
            news.setContent(newsInfoRQ.getContent());
            if (!StringUtils.isEmpty(newsInfoRQ.getNew_source())) {
                news.setNewsSource(newsInfoRQ.getNew_source());
            }
            if (!StringUtils.isEmpty(newsInfoRQ.getImage())) {
                news.setImage(newsInfoRQ.getImage());
            }
            news.setUpdateAt(new Date());
            news.setType(newsInfoRQ.getType());
            newsMapper.updateById(news);
            log.info("文章修改成功");
            return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS);
        } catch (Exception e) {
            e.printStackTrace();
            return ResponseResult.buildResponseResult(ResCodeEnum.SAVE_FAIL);
        }
    }

    @Override
    @Transactional(rollbackFor = Exception.class)
    public ResponseResult deleteNews(int id) {
        try {
            News news = newsMapper.selectById(id);
            if (!ObjectUtils.isEmpty(news)) {
                news.setState(1);
                if(newsMapper.updateById(news) > 0) {
                    log.info("资讯删除成功");
                    return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS);
                } else {
                    log.error("资讯删除失败");
                    return ResponseResult.buildResponseResult(ResCodeEnum.FAILED);
                }
            } else {
                log.error("资讯不存在");
                return ResponseResult.buildResponseResult(ResCodeEnum.ERROR_NOT_FOUND);
            }
        } catch (Exception e) {
            e.printStackTrace();
            log.error("资讯删除失败");
            return ResponseResult.buildResponseResult(ResCodeEnum.FAILED);
        }
    }

    @Override
    @Transactional(rollbackFor = Exception.class)
    public ResponseResult delete(List<Integer> ids) {
        int count = 0;
        try {
            for (Integer id : ids) {
                News news = newsMapper.selectById(id);
                if (!ObjectUtils.isEmpty(news)) {
                    news.setState(1);
                    if(newsMapper.updateById(news) > 0) {
                        count++;
                    }
                }
            }
            log.info("删除资讯成功,成功条数：" + count );
            return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, count);
        } catch (Exception e) {
            e.printStackTrace();
            log.error("资讯删除失败");
            return ResponseResult.buildResponseResult(ResCodeEnum.FAILED);
        }
    }

     @Override
     public ResponseResult getInnerOneNews(int id) {
         try {
             News news = newsMapper.selectById(id);
             if (news.getHits() == null) {
                 news.setHits(1);
             } else {
                 news.setHits(news.getHits()+1);
             }
             newsMapper.updateById(news);
             NewDetailDTO dto= BeanUtils.copyProperties(news,NewDetailDTO.class);
             //查找userid的名字
             PlatformManagers platformManagers = platformManagersService.selectById(news.getUserId());
             NewsInfoDTO newsInfo = new NewsInfoDTO();
             newsInfo.setNews(dto);
             newsInfo.setUsername(platformManagers.getNickname());
             return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, newsInfo);
         } catch (Exception e) {
             e.printStackTrace();
             return ResponseResult.buildResponseResult(ResCodeEnum.FAILED);
         }
     }

     @Override
     public ResponseResult<List<NewsType>> queryNewsTypes() {
         try {
             List<NewsType> newsTypes = newsTypeMapper.selectList(null);
             return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS,newsTypes);
         } catch (Exception e) {
             log.error("查询资讯类型出错",e);
             return ResponseResult.buildResponseResult(ResCodeEnum.FAILED,null);
         }
     }

     @Override
     public ResponseResult addOrUpdateNews(Integer managerId, NewsInfoRQ newsInfoRQ) {
         Integer id = newsInfoRQ.getId();
         try {
             if(id==null){
                 News news = new News();
                 news.setTitle(newsInfoRQ.getTitle())
                     .setContent(newsInfoRQ.getContent())
                     .setNewsSource(newsInfoRQ.getNew_source())
                     .setImage(newsInfoRQ.getImage())
                     .setType(newsInfoRQ.getType())
                     .setCreateAt(new Date())
                     .setUpdateAt(new Date())
                     .setUserId(managerId)
                     .setState(0);
                 newsMapper.insert(news);
                 log.info("资讯创建成功");
             }else {
                 News news = newsMapper.selectById(newsInfoRQ.getId());
                 news.setTitle(newsInfoRQ.getTitle());
                 news.setContent(newsInfoRQ.getContent());
                 if (!StringUtils.isEmpty(newsInfoRQ.getNew_source())) {
                     news.setNewsSource(newsInfoRQ.getNew_source());
                 }
                 if (!StringUtils.isEmpty(newsInfoRQ.getImage())) {
                     news.setImage(newsInfoRQ.getImage());
                 }
                 news.setUpdateAt(new Date());
                 news.setType(newsInfoRQ.getType());
                 newsMapper.updateById(news);
                 log.info("资讯修改成功");
             }
             return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS);
         } catch (Exception e) {
             log.error("保存资讯失败！",e);
             return ResponseResult.buildResponseResult(ResCodeEnum.SAVE_FAIL);
         }
     }
 }
