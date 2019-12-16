package com.bee.platform.user.service.impl;

import com.baomidou.mybatisplus.mapper.EntityWrapper;
import com.baomidou.mybatisplus.mapper.Wrapper;
import com.baomidou.mybatisplus.plugins.pagination.Pagination;
import com.baomidou.mybatisplus.service.impl.ServiceImpl;
import com.bee.platform.common.entity.NoticeTemplate;
import com.bee.platform.common.entity.ResCodeEnum;
import com.bee.platform.common.entity.ResponseResult;
import com.bee.platform.common.enums.NoticeTemplateType;
import com.bee.platform.common.enums.Status;
import com.bee.platform.common.service.NoticeTemplateService;
import com.bee.platform.common.utils.BeanUtils;
import com.bee.platform.common.utils.PageUtils;
import com.bee.platform.user.dao.mapper.SystemNoticeMapper;
import com.bee.platform.user.dto.CreateNoticesDTO;
import com.bee.platform.user.dto.SystemNoticeDTO;
import com.bee.platform.user.entity.SystemNotice;
import com.bee.platform.user.service.SystemNoticeService;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.util.CollectionUtils;

import java.text.MessageFormat;
import java.util.ArrayList;
import java.util.Date;
import java.util.List;

/**
 *  后台管理系统通知相关接口
 *
 * @author junyang.li
 * @since 2019-05-06
 */
@Slf4j
@Service
public class SystemNoticeServiceImpl extends ServiceImpl<SystemNoticeMapper, SystemNotice> implements SystemNoticeService {

    @Autowired
    private SystemNoticeMapper systemNoticeMapper;

    @Autowired
    private NoticeTemplateService noticeTemplateService;

    /**
     * @notes: 列表查询系统通知
     * @Author: junyang.li
     * @Date: 10:29 2019/5/6
     * @param managerId : 当前用户id
     * @param pagination : 分页对象
     * @return: com.bee.platform.common.entity.ResponseResult
     */
    @Override
    public ResponseResult<List<SystemNoticeDTO>> getNoticeList(int managerId,Integer type, Pagination pagination) {
        Wrapper<SystemNotice> wrapper=new EntityWrapper<SystemNotice>()
                .where("notifier_id={0} and status={1}",managerId, Status.TRUE.getKey());
        if(type !=null){
            wrapper.andNew("`read`={0}",type);
        }
        List<SystemNotice> notices=systemNoticeMapper.selectPage(pagination,wrapper.orderBy("`read`").orderBy("create_time",false));
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS,
                BeanUtils.assemble(SystemNoticeDTO.class,notices), PageUtils.transToPage(pagination));
}

    /**
     * @notes: 阅读系统通知
     * @Author: junyang.li
     * @Date: 10:39 2019/5/6
     * @param managerId : 当前用户id
     * @param noticeIds : 系统通知id数组
     * @return: com.bee.platform.common.entity.ResponseResult<com.bee.platform.common.entity.ResCodeEnum>
     */
    @Override
    @Transactional(rollbackFor = Exception.class)
    public ResponseResult<ResCodeEnum> updateNotices(int managerId, List<Integer> noticeIds) {
        //查询未读的系统通知id
        List<Integer> list=systemNoticeMapper.selectNoticeByNotifierId(managerId,noticeIds);
        if(CollectionUtils.isEmpty(list)){
            return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS);
        }
        //将这些系统通知标记为已读
        systemNoticeMapper.update(new SystemNotice().setRead(Status.TRUE.getKey()),new EntityWrapper<SystemNotice>().in("notice_id",list));
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS);
    }

    /**
     * @notes: 新增系统通知并插入到数据库中
     * @Author: junyang.li
     * @Date: 17:37 2019/5/6
     * @param dto : 参数对象
     */
    @Override
    @Transactional(rollbackFor = Exception.class)
    public void createNotices(CreateNoticesDTO dto) {
    }

    /**
     * @notes:  生成并插入一个系统通知
     * @Author: junyang.li
     * @Date: 14:59 2019/5/10
     * @param notifierIds : 通知人id
     * @param templateType : 通知模板类型
     * @param params : 参数
     * @return: void
     */
    @Override
    public void insertNotice(int notifierIds, NoticeTemplateType templateType, Object... params) {
        SystemNotice notice=this.createNotice(notifierIds,templateType,params);
        this.insert(notice);
    }

    /**
     * @notes: 创建新的系统通知并返回一个通知对象
     * @Author: junyang.li
     * @Date: 14:17 2019/5/9
     * @param notifierIds : 通知人id
     * @param templateType : 模板类型
     * @param params : 参数集合
     * @return: com.bee.platform.user.entity.SystemNotice
     */
    @Override
    public SystemNotice createNotice(int notifierIds, NoticeTemplateType templateType, Object... params) {
        //生成站内消息
        NoticeTemplate template=noticeTemplateService.getTemplateByType(templateType);
        //创建站内消息
        String content= MessageFormat.format(template.getTemplateContent(), params);
        return  new SystemNotice().setNotifierId(notifierIds)
                .setNoticeTitle(template.getNoticeTitle()).setNoticeContent(content).setCreateTime(new Date())
                .setRead(Status.FALSE.getKey()).setStatus(Status.TRUE.getKey()).setUpdateTime(new Date());
    }

    /**
     * @notes: 创建新的系统通知并返回 通知对象集合
     * @Author: junyang.li
     * @Date: 14:17 2019/5/9
     * @param notifierIds : 通知人id
     * @param templateType : 模板类型
     * @param params : 参数集合
     * @return: java.util.List<com.bee.platform.user.entity.SystemNotice>
     */
    @Override
    public List<SystemNotice> createNotice(int[] notifierIds, NoticeTemplateType templateType, Object... params) {
        //生成站内消息
        NoticeTemplate template=noticeTemplateService.getTemplateByType(templateType);
        //创建站内消息
        String content= MessageFormat.format(template.getTemplateContent(), params);
        List<SystemNotice> list=new ArrayList<>();
        for (int item:notifierIds) {
            SystemNotice notice=new SystemNotice().setNotifierId(item)
                    .setNoticeTitle(template.getNoticeTitle()).setNoticeContent(content).setCreateTime(new Date())
                    .setRead(Status.FALSE.getKey()).setStatus(Status.TRUE.getKey()).setUpdateTime(new Date());
            list.add(notice);
        }
        return list;
    }

    /**
     * @notes: 批量插入系统通知
     * @Author: junyang.li
     * @Date: 13:42 2019/5/9
     * @param notices : 通知集合
     * @return: void
     */
    @Override
    public void insertAll(List<SystemNotice> notices) {
        if(CollectionUtils.isEmpty(notices)){
            return;
        }
        systemNoticeMapper.insertAll(notices);
    }

    /**
     * @notes: web端消息通知
     * @Author: junyang.li
     * @Date: 10:37 2019/5/9
     * @param notifierIds : 通知人id
     * @param noticeTitle : 通知标题
     * @param content : 通知内容
     * @return: void
     */
    private void webNotice(Integer [] notifierIds,String noticeTitle,String content){
        List<SystemNotice> list=new ArrayList<>();
        for (int item:notifierIds) {
            SystemNotice notice=new SystemNotice().setNotifierId(item)
                    .setNoticeTitle(noticeTitle).setNoticeContent(content).setCreateTime(new Date())
                    .setRead(Status.FALSE.getKey()).setStatus(Status.TRUE.getKey()).setUpdateTime(new Date());
            list.add(notice);
        }
        systemNoticeMapper.insertAll(list);
    }
}
